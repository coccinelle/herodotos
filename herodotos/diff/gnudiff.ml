
exception Unexpected of string

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
	begin
	  ignore(input_line in_ch); (* Read +++ line *)
	  line := Int64.succ !line;
	  let hunks = read_hunks in_ch in
	  hunks
	end
      (* This was when several diff where aggreated in a single patchset *)
      (*
	let rmfile = Str.matched_group 1 line_rm in
	let tail =
	try
	read_diff prefix file in_ch
	with End_of_file -> []
	in
	let ver_file = Misc.strip_prefix prefix rmfile in
	(ver_file, Ast_diff.GNUDiff hunks)::tail
      *)
      else
	begin
	  LOG "Error: Desynchronized? no more diff found in %s at line %s" file (Int64.to_string !line) LEVEL ERROR;
	  LOG "Expects a \"---\" line. Got \"%s\"" line_rm0 LEVEL ERROR;
	  LOG "Looking for filename in \"%s\"" line_rm LEVEL ERROR;
	  match lookfor_minusline in_ch with
	      None ->
		LOG "Unable to re-synchronize" LEVEL ERROR; []
	    | Some l ->
		LOG "Re-synchronize on %s" l LEVEL INFO;
		match lookfor_minusline in_ch with
		    None   -> []
		  | Some l ->
		      seek_in in_ch ((pos_in in_ch) - (1+String.length l));
		      line := Int64.pred !line;
		      read_diff prefix file in_ch
	end

let parse_diff v prefix file : Ast_diff.diffs =
  try    
    let ver_file = Misc.strip_prefix prefix file in
    if Sys.file_exists file then
      begin
	LOG "Parsing GNU Diff: %s" file LEVEL TRACE;
	let in_ch = open_in file in
	try
	  line := Int64.zero;
	  let ast = read_diff prefix file in_ch in
	  close_in in_ch;
	  [(ver_file, Ast_diff.GNUDiff ast)]
	with
	    Misc.Strip msg ->
	      LOG "Strip: %s" msg LEVEL ERROR;
	      close_in in_ch;
	      raise (Unexpected msg)
	  | (Unexpected msg) ->
	    LOG "Unexpected token: %s" msg LEVEL ERROR;
	    close_in in_ch;
	    raise (Unexpected msg)
	  | End_of_file ->
	    LOG "*** DEBUG *** %s is empty !" file LEVEL DEBUG;
	    close_in in_ch;
	  []
      end
    else
      [(ver_file, Ast_diff.DeletedFile)]
  with Sys_error msg ->
    LOG "*** WARNING *** %s" msg LEVEL WARN;
    []

let compute_new_pos_with_hunks hunks line colb cole =
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
    (Ast_diff.Sing nline, colb, cole)
  else
	  (*
	    We are IN the hunk.
	  *)
    if asize = 0 then
      (Ast_diff.Deleted false, 0, 0)
    else
	    (*
	      We are maybe in the set of replacing lines !?
	    *)
      (Ast_diff.Cpl (al,al+asize-1),colb,cole)

let compute_new_pos_with_findhunk (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  Debug.profile_code_silent "Gnudiff.compute_new_pos_with_findhunk"
    (fun () ->
      let (line, colb, cole) = pos in
      try
	match List.assoc (ver, file) diffs with
	    Ast_diff.GNUDiff hunks -> (false, compute_new_pos_with_hunks hunks line colb cole)
	  | Ast_diff.DeletedFile -> (true, (Ast_diff.Unlink, 0, 0))
	  | _ -> raise (Unexpected "Wrong diff type")
      with Not_found -> (false, (Ast_diff.Sing line, colb, cole))
    )
