open Lexing

exception Unexpected of string
exception Break

let line = ref Int64.zero
(* let diffcmd = "diff -Ebw -U0 " *)
let diffcmd = "diff -U0 "

let nonewline_re = Str.regexp "^\\\\ "

let read_pos pos =
  let pos_re = Str.regexp "^\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?$" in
  let posb = Str.string_match pos_re pos 0 in
    if posb then
      let linepos = int_of_string (Str.matched_group 1 pos) in
      let linenum =
	try int_of_string (Str.matched_group 3 pos)
	with Not_found -> 1
      in
	(linepos, linenum)
    else
      raise (Unexpected ("bad hunk position format at line "^Int64.to_string !line))

let rec skip in_ch lineskip =
  if lineskip > 0 then
    begin
      let linetxt = input_line in_ch in
	line := Int64.succ !line;
	if Str.string_match nonewline_re linetxt 0 then
	    skip in_ch (lineskip)
	else
	  skip in_ch (lineskip -1)
    end

let rec read_hunks in_ch =
  let hunk_desc = input_line in_ch in
  let hunk_re = Str.regexp "^@@ -\\([^ ]+\\) \\+\\([^ ]+\\) @@$" in
  let hunkb = Str.string_match hunk_re hunk_desc 0 in
    if hunkb then
      begin
	line := Int64.succ !line;
	let before = read_pos (Str.matched_group 1 hunk_desc) in
	  ignore(Str.string_match hunk_re hunk_desc 0);
	  let after = read_pos (Str.matched_group 2 hunk_desc) in
	    skip in_ch ((snd before) + (snd after));
	    let tail =
	      try
		read_hunks in_ch
	      with End_of_file -> []
	    in
	      (before, after)::tail
      end
    else
      begin
	seek_in in_ch ((pos_in in_ch) - (1+String.length hunk_desc));
	[]
      end

let rec lookfor_minusline in_ch =
  try
    let line_rm0 = input_line in_ch in
    let tab_re = Str.regexp "\t" in
    let rm_re = Str.regexp "^--- \\([^ ]+\\) .*$" in
    let line_rm = Str.global_replace tab_re " " line_rm0 in
    let rmb = Str.string_match rm_re line_rm 0 in
    line := Int64.succ !line;
      if rmb then
	Some line_rm0
      else
	lookfor_minusline in_ch
  with _ -> None

let rec read_diff prefix file in_ch =
  let line_rm00 = input_line in_ch in
  let line_rm0 =
    if Str.string_match nonewline_re line_rm00 0
    then
      (line := Int64.succ !line; input_line in_ch)
    else
      line_rm00
  in
    line := Int64.succ !line;
    let tab_re = Str.regexp "\t" in
    let rm_re = Str.regexp "^--- \\([^ ]+\\) .*$" in
    let line_rm = Str.global_replace tab_re " " line_rm0 in
    let rmb = Str.string_match rm_re line_rm 0 in
      if rmb then
	let rmfile = Str.matched_group 1 line_rm in
	  ignore(input_line in_ch); (* Read +++ line *)
	  line := Int64.succ !line;
	  let hunks = read_hunks in_ch in
	  let tail =
	    try
	      read_diff prefix file in_ch
	    with End_of_file -> []
	  in
	  let ver_file = Misc.strip_prefix prefix rmfile in
	    (ver_file, hunks)::tail
      else
	begin
	  prerr_endline
	    ("Error: Desynchronized? no more diff found in "^file
	     ^ " at line "^Int64.to_string !line);
	  prerr_endline
	    ("Expects a \"---\" line. Got \""^line_rm0^"\"");
	  prerr_endline
	    ("Looking for filename in \""^line_rm^"\"");
	  match lookfor_minusline in_ch with
	      None ->
		prerr_endline ("Unable to re-synchronize"); []
	    | Some l ->
		prerr_endline ("Re-synchronize on "^l);
		match lookfor_minusline in_ch with
		    None   -> []
		  | Some l ->
		      seek_in in_ch ((pos_in in_ch) - (1+String.length l));
		      line := Int64.pred !line;
		      read_diff prefix file in_ch
	end

let parse_diff v prefix file : Ast_diff.diffs =
  try
    let in_ch = open_in file in
      try
	line := Int64.zero;
	let ast = read_diff prefix file in_ch in
	  close_in in_ch;
	  ast
      with
	  (Unexpected msg) ->
	    prerr_endline ("Unexpected token: "^msg);
	    close_in in_ch;
	    raise (Unexpected msg)
	| End_of_file ->
	    if v then prerr_endline ("*** WARNING *** "^file^" is empty !");
	    close_in in_ch;
	    []
  with Sys_error msg ->
    prerr_endline ("*** WARNING *** "^msg);
    []

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

let get_diff v prefix vlist (orgs: Ast_org.orgarray) orgfile file : Ast_diff.diffs =
  let orgstat = (Unix.stat orgfile).Unix.st_ctime in
  let patchstat =
    if Sys.file_exists file
    then (Unix.stat file).Unix.st_ctime
    else orgstat
  in

  if Sys.file_exists file && orgstat < patchstat then
    parse_diff v prefix file
  else
    (
      if orgstat > patchstat
      then prerr_endline ("*** RECOMPUTE *** " ^file)
      else prerr_endline ("*** COMPUTE *** " ^file);
      let pair = Misc.unique_list (gen_diff prefix vlist orgs) in
	ignore (Unix.system ("> "^file));
	List.iter
	  (fun (ofile, nfile) ->
	     let cmd = diffcmd ^ofile ^" "^ nfile ^ " >> "^ file in
	       match
		 Unix.system cmd
	       with
		   Unix.WEXITED 0 -> ()
		 | Unix.WEXITED 1 -> ()
		 | Unix.WEXITED i -> prerr_endline ("*** FAILURE *** Code:" ^(string_of_int i) ^" "^ cmd)
		 | _ -> prerr_endline ("*** FAILURE *** " ^cmd)
	  ) pair ;
	parse_diff v prefix file
    )

type lineprediction =
    Deleted
  | Sing of int
  | Cpl of int * int

(* Old implementation *)
let compute_new_pos_with_rec (diffs: Ast_diff.diffs) file ver pos : lineprediction * int * int =
  let (orig_line, o_colb, o_cole) = pos in
  let o_pos = (Sing orig_line, o_colb, o_cole) in
  try
(*    prerr_string " - as new line:"; *)
    let hunks = List.assoc (ver, file) diffs in
    let newhunks = List.map
      (fun p ->
	 let ((bl,bsize),(al,asize)) = p in
	   if bsize = 0 then
	     ((bl+1,bsize),(al,asize))
	   else if asize = 0 then
	     ((bl,bsize),(al+1,asize))
	   else
	     p
      ) hunks
    in
    let new_pos = List.fold_left
      (fun (wrap_line, colb, cole) ((bl,bsize),(al,asize)) ->
	 match wrap_line with
	     Sing line ->
	       (* Occurrence is known to be at a exact line *)
	       if  bl <= orig_line then
		 (* Hunks before orig_line are interesting *)
		 if (bl+bsize-1) < orig_line then
		   (* orig_line is after the modified lines *)
		   let nline = line + asize - bsize in
		     (*	     prerr_string (" " ^string_of_int nline);*)
		     (Sing nline, colb, cole)
		 else if asize = 0 then
		   (*
		     orig_line is IN the current hunk.
		     '+' part is empty. All hunk lines are removed.
		   *)
		   (Deleted, 0, 0)
		 else
		   (*
		      We are IN the hunk,
		      give the set of lines in '+' part
		   *)
		   (Cpl (al,al+asize-1),colb,cole)
	       else (* bl > orig_line, remaining hunks are not interesting *)
		 (*
		   Return the current value.
		 *)
		 (wrap_line, colb, cole)
	   | Deleted | Cpl _  ->
	       (wrap_line, colb, cole)
      ) o_pos newhunks
    in (* prerr_newline (); *) new_pos
  with Not_found -> o_pos

let compute_new_pos_with_findhunk (diffs: Ast_diff.diffs) file ver pos : lineprediction * int * int =
  let (line, colb, cole) = pos in
    try
      let hunks = List.assoc (ver, file) diffs in
      let newhunks = List.map
	(fun p ->
	   let ((bl,bsize),(al,asize)) = p in
	     if bsize = 0 then
	       ((bl+1,bsize),(al,asize))
	     else if asize = 0 then
	       ((bl,bsize),(al+1,asize))
	     else
	       p
	) hunks
      in
      let hunk = List.fold_left
	(fun p1 p2 ->
	   let ((bl1,_),_) = p1 in
	   let ((bl2,_),_) = p2 in
	     if bl1 <= line && line < bl2 then p1
	     else p2
	) ((0,0),(0,0)) newhunks
      in
      let ((bl,bsize),(al,asize)) = hunk in
	if (bl+bsize) <= line then
	  (*
	    If we are above the current hunk, but still before the next,
	    we computes the prediction by adding the two offsets.
	  *)
	  let nline = line + (al - bl) + (asize - bsize) in
	    (Sing nline, colb, cole)
	else
	  (*
	    We are IN the hunk.
	  *)
	  if asize = 0 then
	    (Deleted, 0, 0)
	  else
	    (*
	      We are maybe in the set of replacing lines !?
	    *)
	    (Cpl (al,al+asize-1),colb,cole)
    with Not_found -> (Sing line, colb, cole)

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : lineprediction * int * int =
  let my_compute_new_pos =
    if true then
      compute_new_pos_with_findhunk
    else
      compute_new_pos_with_rec
  in my_compute_new_pos diffs file ver pos

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
