open Lexing

exception UnsupportedDiff of string
exception Break

type difftype =
    GNUDiff of string
  | Gumtree of string

let selected_compute_new_pos = ref Gnudiff.compute_new_pos_with_findhunk

let get_basetime orgstat patchfile =
  if Sys.file_exists patchfile
  then (Unix.stat patchfile).Unix.st_ctime
  else orgstat

let is_GNUdiff difffile =
  match difffile with
      GNUDiff _ -> true
    | Gumtree _ -> false

let get_difffile difffile =
  match difffile with
      GNUDiff file -> file
    | Gumtree file -> file

let get_diffcmd prefix ofile nfile difffile =
  match difffile with
      GNUDiff file -> (file, Gnudiff.diffcmd ^ofile ^" "^ nfile ^ " >> "^ file)
    | Gumtree file ->
        let (ver, stripped_ofile) = Misc.strip_prefix prefix ofile in
	let outfile = String.concat Filename.dir_sep [file; ver; stripped_ofile] in
	(outfile,
	 "mkdir -p " ^ Filename.dirname outfile ^" && " ^
	   Gumtree.diffcmd ^ofile ^" "^ nfile ^ " > "^ outfile)

let parse_diff v prefix difffile : Ast_diff.diffs =
  match difffile with
      GNUDiff file -> Gnudiff.parse_diff v prefix file
    | Gumtree file -> Gumtree.parse_diff v prefix file

let select_diff diffalgo bugfile : difftype =
  let (proto, file) =
    match Str.split (Str.regexp_string ":") diffalgo with
	[] -> ("diff", diffalgo)
      | proto::fileparts ->
	let file = String.concat "" fileparts in
	(proto, file)
  in
  match proto with
      "diff" ->
	if file = "" then GNUDiff (bugfile ^ Global.patchext)
	else GNUDiff file
    | "gumtree" ->
      selected_compute_new_pos := Gumtree.compute_new_pos_with_gumtree;
	if file = "" then Gumtree (bugfile ^ Global.patchext)
	else Gumtree file
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

let get_diff_job v prefix orgstat difffile ofile nfile =
  let (outfile, cmd) = get_diffcmd prefix ofile nfile difffile in
  let patchstat = get_basetime orgstat outfile in
  if orgstat > patchstat then
    begin
      if v then prerr_endline ("Running: " ^cmd);
      match
	Unix.system cmd
      with
	  Unix.WEXITED 0 -> ()
	| Unix.WEXITED 1 -> ()
	| Unix.WEXITED i -> prerr_endline ("*** FAILURE *** Code:" ^(string_of_int i) ^" "^ cmd)
	| _ -> prerr_endline ("*** FAILURE *** " ^cmd)
    end

let get_diff_nofail v prefix orgstat difffile (ofile, nfile) =
  try
    get_diff_job v prefix orgstat difffile ofile nfile;
    0
  with Config.Warning msg ->
    prerr_endline ("*** WARNING *** " ^ msg);
    1

let run_get_diff_job v prefix orgstat difffile file_pair =
  let (ofile, nfile) = file_pair in
  let pid = Unix.fork () in
    if pid = 0 then (* I'm a slave *)
      begin
	let pid = Unix.getpid() in
	  if !Misc.debug then prerr_endline ("New child "^ string_of_int pid^ " on "^ofile ^"/"^nfile);
	  let ret = get_diff_nofail v prefix orgstat difffile file_pair in
	    if !Misc.debug then prerr_endline ("Job done for child "^ string_of_int pid);
	    let msg = Debug.profile_diagnostic () in
	      if msg <> "" then prerr_endline msg;
	    exit ret
      end
    else (* I'm the master *)
      pid

let dispatch_get_diff_job v cpucore prefix orgstat difffile (perr, pidlist) file_pair =
  let (error, newlist) =
    if List.length pidlist > cpucore then
      let (death, status) = Unix.wait () in
	if !Misc.debug then
	  prerr_endline ("Master: Job done for child "^ string_of_int death);
	let error = match status with
	    Unix.WEXITED 0 -> perr
	  | _              -> perr + 1
	in
	(error, List.filter (fun x -> x <> death) pidlist)
    else
      (perr, pidlist)
  in
  let pid = run_get_diff_job v prefix orgstat difffile file_pair in
    (error, pid::newlist)

let get_diff v cpucore resultsdir pdir prefix vlist (orgs: Ast_org.orgarray) orgfile difffile : Ast_diff.diffs =
  let file = get_difffile difffile in
  let orgstat = Misc.get_change_stat resultsdir pdir vlist orgfile (ref []) in
  let patchstat = get_basetime orgstat file in

  if Sys.file_exists file
    && is_GNUdiff difffile
    && orgstat < patchstat then
    parse_diff v prefix difffile
  else
    (
      if orgstat > patchstat
      then prerr_endline ("*** RECOMPUTE *** " ^file)
      else prerr_endline ("*** COMPUTE *** " ^file);
      let pair = Misc.unique_list (gen_diff prefix vlist orgs) in
      (match difffile with
	GNUDiff file ->
	  ignore (Unix.system ("> "^file));
	  List.iter
	    (fun (ofile, nfile) ->
	      let (_, cmd) = get_diffcmd prefix ofile nfile difffile in
	      if v then prerr_endline ("Running: " ^cmd);
	      match
		Unix.system cmd
	      with
		  Unix.WEXITED 0 -> ()
		| Unix.WEXITED 1 -> ()
		| Unix.WEXITED i -> prerr_endline ("*** FAILURE *** Code:" ^(string_of_int i) ^" "^ cmd)
		| _ -> prerr_endline ("*** FAILURE *** " ^cmd)
	    ) pair ;
	  parse_diff v prefix difffile
      | Gumtree file ->
	Unix.mkdir file 0o770;
	prerr_endline ("*** CREATING DIRECTORY *** " ^file);
	let error =
	  if cpucore = 1 then
	    let errs = List.map (get_diff_nofail v prefix orgstat difffile) pair in
	    List.fold_left (+) 0 errs
	  else
	    let (err, pidlist) =
	      List.fold_left
		(dispatch_get_diff_job v cpucore prefix orgstat difffile)
		(0, [])
		pair
	    in
	    let res = List.map (fun x ->
	      let (death, status) = Unix.wait () in
	      if !Misc.debug then
		prerr_endline ("Master: Job done for child "^ string_of_int death);
	      match status with
		  Unix.WEXITED 0 -> 0
		| _ -> 1
	    ) pidlist
	    in
	    List.fold_left (+) err res
	in
	if error <> 0 then
	  prerr_endline ("*** ERROR *** "^string_of_int error ^" error(s) during the gumtree diff.");
	parse_diff v prefix difffile
      )
    )

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : Ast_diff.lineprediction * int * int =
  Debug.profile_code_silent "Diff.compute_new_pos"
    (fun () ->
      !selected_compute_new_pos diffs file ver pos
    )

let show_gnudiff hunks =
  List.iter (fun ((bl,bsize),(al,asize)) ->
    prerr_int bl;
    prerr_string "(";
    prerr_int bsize;
    prerr_string ") -> ";
    prerr_int al;
    prerr_string "(";
    prerr_int asize;
    prerr_string "), "
  ) hunks

let show_diff verbose vlist ast =
  if verbose then
    begin
      prerr_endline "SHOW DIFF";
      List.iter (fun ((ver, file), difftype) ->
		   prerr_string file;
		   prerr_string " from ";
		   prerr_string ver;
		   prerr_string " to ";
		   prerr_string (Misc.get_next_version vlist ver);
		   prerr_endline "";
		   (match difftype with
		       Ast_diff.GNUDiff hunks -> show_gnudiff hunks
		     | _ -> raise (UnsupportedDiff ""));
		   prerr_endline ""
		) ast
    end
