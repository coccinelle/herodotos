open Lexing
open Diff_type

exception UnsupportedDiff of string
exception Break

let selected_compute_new_pos = ref Gnudiff.compute_new_pos_with_findhunk

let selected_alt_new_pos = ref None

let get_basetime orgstat patchfile =
  if Sys.file_exists patchfile
  then (Unix.stat patchfile).Unix.st_mtime
  else neg_infinity (* To force diff-ing tool *)

let is_GNUdiff difffile =
  match difffile with
      GNUDiff _ -> true
    | _ -> false

let is_hybrid difffile =
  match difffile with
      Hybrid _ -> true
    | _ -> false

let get_difffile difffile =
  match difffile with
      GNUDiff file -> file
    | Gumtree file -> file
    | Hybrid  file -> file 

let get_diffcmd prefix ofile nfile difffile =
  let (ver, stripped_ofile) = Misc.strip_prefix prefix ofile in
  let file = get_difffile difffile in
  let outfile = String.concat Filename.dir_sep [file; ver; stripped_ofile] in
  let cmd = match difffile with
      GNUDiff _ -> Gnudiff.diffcmd ^ofile ^" "^ nfile ^ " > "^ outfile
    | Gumtree _ -> Gumtree.diffcmd ^ofile ^" "^ nfile ^ " | gum2hero "^ outfile
    | Hybrid _ -> raise (UnsupportedDiff "Hybrid should be instanciated at this point.")
  in (outfile, "mkdir -p " ^ Filename.dirname outfile ^" && " ^ cmd)

let parse_diff v prefix difffile : Ast_diff.diffs =
  match difffile with
      GNUDiff file -> Gnudiff.parse_diff v prefix file
    | Gumtree file -> Gumtree.parse_diff v prefix file
    | Hybrid _ -> raise (UnsupportedDiff "Hybrid should be instanciated at this point.")

let select_diff diffalgo project : difftype =
  let (proto, file) =
    match Str.split (Str.regexp_string ":") diffalgo with
	[] -> ("gnudiff", diffalgo)
      | proto::fileparts ->
	let file = String.concat "" fileparts in
	(proto, file)
  in
  match proto with
      "gnudiff" ->
	if file = "" then GNUDiff (project ^ Global.patchext)
	else GNUDiff file
    | "gumtree" ->
      selected_compute_new_pos := Gumtree.compute_new_pos_with_gumtree;
      if file = "" then Gumtree (project ^ Global.gumtreeext)
      else Gumtree file
    | "hybrid" ->
      selected_compute_new_pos := Hybrid.compute_new_pos;
      selected_alt_new_pos := Some (Hybrid.alt_new_pos);
      Hybrid (project)
    | _ -> raise (UnsupportedDiff (proto ^ " is unsupported as a diff algorithm."))


let rec gen_diff_of_orglist prefix vlist orgs : (string * string) list =
  match orgs with
      []       -> []
    | hd::tail ->
	let (_, _, _, file, ver, _, _, _, _, _,_) = hd in
	let nver = Misc.get_next_version vlist ver in
	  if nver = "" then
	    gen_diff_of_orglist prefix vlist tail
	  else
	    let ofile = prefix ^ ver ^ "/" ^ file in
	    let nfile = prefix ^ nver ^ "/" ^ file in
	      if Sys.file_exists ofile && Sys.file_exists nfile then
		(ofile,nfile)::gen_diff_of_orglist prefix vlist tail
	      else
		gen_diff_of_orglist prefix vlist tail

let gen_diff prefix vlist (orgarray: Ast_org.orgarray) : (string * string) list =
  Array.fold_left
    (fun head (flist, tbl) ->
       head @ (List.flatten (List.map
		 (fun file ->
		    gen_diff_of_orglist prefix vlist (Hashtbl.find tbl file)
		 ) flist
			    ))
       ) [] orgarray

let get_diff_job v prefix difffile cmd =
  LOG "Running: %s" cmd LEVEL TRACE;
  match
    Unix.system cmd
  with
      Unix.WEXITED 0 -> ()
    | Unix.WEXITED 1 -> ()
    | Unix.WEXITED i -> LOG "*** FAILURE *** Code: %d %s" i cmd LEVEL ERROR
    | _ -> LOG "*** FAILURE *** %s" cmd LEVEL ERROR

let get_diff_nofail v prefix difffile cmd =
  try
    get_diff_job v prefix difffile cmd;
    0
  with Config.Warning msg ->
    LOG "*** WARNING *** %s" msg LEVEL WARN;
    1

let run_get_diff_job v prefix difffile cmd =
  let pid = Unix.fork () in
    if pid = 0 then (* I'm a slave *)
      begin
	let pid = Unix.getpid() in
	LOG "New child %d for %s" (Unix.getpid ()) cmd LEVEL TRACE;
	let ret = get_diff_nofail v prefix difffile cmd in
	LOG "Job done for child %d" (Unix.getpid ()) LEVEL TRACE;
	let msg = Debug.profile_diagnostic () in
	if msg <> "" then Debug.trace msg;
	exit ret
      end
    else (* I'm the master *)
      pid

let dispatch_get_diff_job v cpucore prefix difffile (perr, pidlist) cmd =
  let (error, newlist) =
    if List.length pidlist > cpucore then
      let (death, status) = Unix.wait () in
      LOG "Master: Job done for child %d" death LEVEL TRACE;
      let error = match status with
	  Unix.WEXITED 0 -> perr
	| _              -> perr + 1
      in
      (error, List.filter (fun x -> x <> death) pidlist)
    else
      (perr, pidlist)
  in
  let pid = run_get_diff_job v prefix difffile cmd in
    (error, pid::newlist)

let gen_cmd_basic v prefix pair orgstat difffile =
  LOG "*** CHECK CACHE *** %s" (get_difffile difffile) LEVEL INFO;
  Parmap.parfold (fun file_pair (outlist, cmdlist) ->
    let (ofile, nfile) = file_pair in
    let (outfile, cmd) = get_diffcmd prefix ofile nfile difffile in
    (* FIXME: This should not longer be based on get_basetime but on file_exists. *)
    let patchstat = get_basetime orgstat outfile in
    if orgstat > patchstat then
      (LOG "Checking (%d) %s - Keep" (Unix.getpid ()) outfile LEVEL TRACE;
       (outfile::outlist,(outfile, cmd)::cmdlist))
    else
      (LOG "Checking (%d) %s - Skip" (Unix.getpid ()) outfile LEVEL TRACE;
       (outfile::outlist,cmdlist))
  ) (Parmap.L pair)
    ([],[])                                  (* Init. *)
    (fun (x1,x2) (y1, y2) -> (x1@y1, x2@y2)) (* Merge *)

let gen_cmd v prefix pair orgstat difffile =
  if is_hybrid difffile then
    let file = get_difffile difffile in
    let gnudiff = gen_cmd_basic v prefix pair orgstat (GNUDiff (file^ Global.patchext)) in
    let gumtree = gen_cmd_basic v prefix pair orgstat (Gumtree (file^ Global.gumtreeext)) in
    let gumoutlist = fst gumtree in
    let gumcmdlist = snd gumtree in
    Hybrid.register_cmd gumcmdlist;
    (fst gnudiff @ gumoutlist, snd (List.split (snd gnudiff))) (* We do not run gumtree cmd here !*)
  else
    let (one, two) = gen_cmd_basic v prefix pair orgstat difffile in
    (one, snd (List.split two))

let mkdir_cache_basic file =
  if not (Sys.file_exists file) then
    (Unix.mkdir file 0o770;
     LOG "*** CREATING DIRECTORY *** %s" file LEVEL INFO)

let mkdir_cache resultsdir pdir vlist orgfile difffile =
  let file = get_difffile difffile in
  if is_hybrid difffile then
    let file = get_difffile difffile in
    let orgstatpatch = mkdir_cache_basic (file^ Global.patchext) in
    let orgstatgumtree = mkdir_cache_basic (file^ Global.gumtreeext) in
    max orgstatgumtree orgstatpatch
  else
    mkdir_cache_basic file

let get_diff v cpucore resultsdir pdir prefix vlist (orgs: Ast_org.orgarray) orgfile difffile : Ast_diff.diffs =
  mkdir_cache resultsdir pdir vlist orgfile difffile;
  let orgstat = Misc.get_change_stat resultsdir pdir vlist orgfile (ref []) in
  let file = get_difffile difffile in
  let pair = Misc.unique_list (gen_diff prefix vlist orgs) in
  let (outfiles, cmds) = gen_cmd v prefix pair orgstat difffile in
  let re = Str.regexp_string "&&" in
  let (dirs, cleaned_cmds) =
    List.fold_left
      (fun (dir_list, cmd_list) x ->
	let (dir, cmd) = match Str.split re x with
	    dir::[cmd] -> (dir, cmd)
	  | _ -> failwith ("Wrong command: x")
	in
	let dirs = if not (List.mem dir dir_list) then
	    dir::dir_list else dir_list
	in
	let cmds = if not (List.mem cmd cmd_list) then
	    cmd::cmd_list else cmd_list
	in (dirs, cmds)
      )
      ([],[]) cmds
  in
  List.iter (fun cmd ->
    match
      Unix.system cmd
    with
	Unix.WEXITED 0 -> ()
      | Unix.WEXITED 1 -> ()
      | Unix.WEXITED i -> LOG "*** FAILURE *** Code: %d %s" i cmd LEVEL ERROR
      | _ -> LOG "*** FAILURE *** %s" cmd LEVEL ERROR
  ) dirs;
  let error =
    if cpucore = 1 then
      let errs = List.map (get_diff_nofail v prefix difffile) cleaned_cmds in
      List.fold_left (+) 0 errs
    else
      let (err, pidlist) =
	List.fold_left
	  (dispatch_get_diff_job v cpucore prefix difffile)
	  (0, [])
	  cleaned_cmds
      in
      let res = List.map (fun x ->
	let (death, status) = Unix.wait () in
	LOG "Master: Job done for child %d" death LEVEL TRACE;
	match status with
	    Unix.WEXITED 0 -> 0
	  | _ -> 1
      ) pidlist
      in
      List.fold_left (+) err res
  in
  if error <> 0 then
    LOG "*** ERROR *** %d error(s) during the diff." error LEVEL ERROR;
  match difffile with
      GNUDiff _ ->
	List.flatten (
	  Parmap.parmap (fun x ->
	    try
	      parse_diff v (file^Filename.dir_sep) (GNUDiff x)
	    with e -> LOG "Error parsing %s" x LEVEL ERROR; raise e
	  ) (Parmap.L outfiles)
	)
    | Gumtree _ -> 
      List.flatten (
	Parmap.parmap (fun x ->
	  try
	    parse_diff v (file^Filename.dir_sep) (Gumtree x)
	  with e -> LOG "Error parsing %s" x LEVEL ERROR; raise e
	) (Parmap.L outfiles)
      )
    | Hybrid _ -> Hybrid.parse_config v file; []

let alt_new_pos (diffs: Ast_diff.diffs) file ver pos : (bool * (Ast_diff.lineprediction * int * int)) option =
  Debug.profile_code_silent "Diff.alt_new_pos"
    (fun () ->
      match !selected_alt_new_pos with
	  None -> None
	| Some alt_new_pos -> Some (alt_new_pos diffs file ver pos)
    )

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  Debug.profile_code_silent "Diff.compute_new_pos"
    (fun () ->
      !selected_compute_new_pos diffs file ver pos
    )

let show_gnudiff hunks =
  String.concat ", "
    (List.map (fun ((bl,bsize),(al,asize)) ->
      Printf.sprintf "%d(%d) -> %d(%d)" bl bsize al asize
     ) hunks
    )

let show_diff verbose vlist ast =
  LOG "SHOW DIFF" LEVEL TRACE;
  List.iter (fun ((ver, file), difftype) ->
    LOG "%s from %s to %s" file ver (Misc.get_next_version vlist ver) LEVEL TRACE;
    match difftype with
	Ast_diff.GNUDiff hunks -> LOG "%s" (show_gnudiff hunks) LEVEL TRACE
      | Ast_diff.Gumtree root -> Gumtree.show_gumtree true 0 root
      | Ast_diff.DeletedFile -> LOG "(deleted)" LEVEL TRACE;
  ) ast
