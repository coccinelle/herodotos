open Lexing

exception UnsupportedDiff of string
exception Break

type difftype =
    GNUDiff of string
  | Gumtree of string

let get_difffile difffile =
  match difffile with
      GNUDiff file -> file
    | Gumtree file -> file

let get_diffcmd ofile nfile difffile =
  match difffile with
      GNUDiff file -> Gnudiff.diffcmd ^ofile ^" "^ nfile ^ " >> "^ file
    | Gumtree file -> Gumtree.diffcmd ^ofile ^" "^ nfile ^ " >> "^ file

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
    | "gumtree" -> Gumtree file
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

let get_diff v resultsdir pdir prefix vlist (orgs: Ast_org.orgarray) orgfile difffile : Ast_diff.diffs =
  let file = get_difffile difffile in
  let orgstat = Misc.get_change_stat resultsdir pdir vlist orgfile (ref []) in
  let patchstat =
    if Sys.file_exists file
    then (Unix.stat file).Unix.st_ctime
    else orgstat
  in

  if Sys.file_exists file && orgstat < patchstat then
    parse_diff v prefix difffile
  else
    (
      if orgstat > patchstat
      then prerr_endline ("*** RECOMPUTE *** " ^file)
      else prerr_endline ("*** COMPUTE *** " ^file);
      let pair = Misc.unique_list (gen_diff prefix vlist orgs) in
	ignore (Unix.system ("> "^file));
	List.iter
	  (fun (ofile, nfile) ->
	     let cmd = get_diffcmd ofile nfile difffile in
	       match
		 Unix.system cmd
	       with
		   Unix.WEXITED 0 -> ()
		 | Unix.WEXITED 1 -> ()
		 | Unix.WEXITED i -> prerr_endline ("*** FAILURE *** Code:" ^(string_of_int i) ^" "^ cmd)
		 | _ -> prerr_endline ("*** FAILURE *** " ^cmd)
	  ) pair ;
	parse_diff v prefix difffile
    )

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : Ast_diff.lineprediction * int * int =
  Debug.profile_code_silent "Diff.compute_new_pos"
    (fun () ->
  let my_compute_new_pos =
    if true then
      Gnudiff.compute_new_pos_with_findhunk
    else
      Gumtree.compute_new_pos_with_gumtree
  in my_compute_new_pos diffs file ver pos
    )

let show_diff verbose vlist ast =
  if verbose then
    begin
      prerr_endline "SHOW DIFF";
      List.iter (fun ((ver, file), hunks) ->
		   prerr_string file;
		   prerr_string " from ";
		   prerr_string ver;
		   prerr_string " to ";
		   prerr_string (Misc.get_next_version vlist ver);
		   prerr_endline "";
		   List.iter (fun ((bl,bsize),(al,asize)) ->
				prerr_int bl;
				prerr_string "(";
				prerr_int bsize;
				prerr_string ") -> ";
				prerr_int al;
				prerr_string "(";
				prerr_int asize;
				prerr_string "), "
			     ) hunks;
		   prerr_endline ""
		) ast
    end
