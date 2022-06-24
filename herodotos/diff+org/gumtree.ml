
exception Unexpected of string

let diffcmd = "gumtree --output asrc "

let parse_diff v prefix file =
  let ver_file = Misc.strip_prefix prefix file in
  if Sys.file_exists file then
    begin
      let logmsg=Printf.sprintf "Parsing Gumtree diff: %s" file in
      Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
      let x = open_in_bin file in
      try
	let tree = input_value x in
	close_in x;
	[(ver_file, Ast_diff.Gumtree (tree))]
      with
	Misc.Strip msg ->
         (let logmsg=Printf.sprintf "Strip: %s" msg in
	    Bolt.Logger.log "" Bolt.Level.ERROR logmsg);
	    close_in x;
	    raise (Unexpected msg)
	| e ->
	  Printexc.print_backtrace stderr;
	  let newfile = file ^ Global.failed in
          let logmsg=Printf.sprintf "Failed while parsing: check %s" newfile in
	  Bolt.Logger.log "" Bolt.Level.ERROR logmsg;
	  Sys.rename file newfile;
	  []
    end
  else
    [(ver_file, Ast_diff.DeletedFile)]

let get_pos_before tree =
  let Ast_diff.Tree(pos_before, _, _) = tree in
  pos_before

let get_pos_after tree =
  let Ast_diff.Tree(_, pos_after, _) = tree in
  pos_after

let get_children tree =
  let Ast_diff.Tree(_, _, children) = tree in
  children

(* Pretty-printing *)
let show_gumtree_pos (_, _, bl, bc, el, ec) =
  Printf.sprintf "%d:%d-%d:%d" bl bc el ec

let rec show_gumtree dorec depth tree =
  let before = get_pos_before tree in
  let oafter = get_pos_after tree in
  let children = get_children tree in
  let logmsg=Printf.sprintf "%s- %s -> %s"
    (String.make depth ' ')
    (show_gumtree_pos before)
    (match oafter with
	Some after -> show_gumtree_pos after
      | None -> "XXX"
    )
    in
  Bolt.Logger.log ""
    Bolt.Level.TRACE logmsg;
  if dorec then
    List.iter (show_gumtree dorec (depth +1)) children

(* *)
let is_perfect pos (tree:Ast_diff.tree) =
  let (line, colb, cole) = pos in
  let (_, _, bl, bc, el, ec)  = get_pos_before tree in
  line = bl && line = el
       && colb = bc && cole = ec

let found tree =
  show_gumtree false 0 tree;
  Bolt.Logger.log "" Bolt.Level.TRACE "-----------------";
  true
 
let match_tree pos (tree:Ast_diff.tree) =
  let (line, colb, cole) = pos in
  let (_, _, bl, bc, el, ec)  = get_pos_before tree in
  if line > bl && (line < el || line = el && cole <= ec) then
    (* Match a node that start before, but may end at the right line, or after *)
    (if !Misc.debug then Bolt.Logger.log "" Bolt.Level.TRACE "match_tree: case 1";
     found tree)
  else if line = bl && line < el
       && colb >= bc then
    (* Match a *large* node that start at the right line. The begin column must be right too. *)
    (if !Misc.debug then Bolt.Logger.log "" Bolt.Level.TRACE "match_tree: case 2";
     found tree)
  else if line = bl && line = el
	       && colb >= bc && cole <= ec then
    (* The perfect line matching is not capture in the previous case.
       We check it here, and also check the columns.
    *)
    (if !Misc.debug then Bolt.Logger.log "" Bolt.Level.TRACE "match_tree: case 3";
     found tree)
  else
    (if !Misc.debug then Bolt.Logger.log "" Bolt.Level.TRACE "match_tree: case 4";
     false)

let rec lookup_tree ver file pos (tree:Ast_diff.tree) : Ast_diff.tree =
  Debug.profile_code_silent "Gumtree.lookup_tree"
    (fun () ->
      let children = get_children tree in
      try
	let candidate = List.find (match_tree pos) children in
	if is_perfect pos candidate then
	  (Bolt.Logger.log "" Bolt.Level.TRACE "lookup_tree: perfect";
	   candidate)
	else
	  (Bolt.Logger.log "" Bolt.Level.TRACE "lookup_tree: !perfect - recurse";
	   lookup_tree ver file pos candidate)
      with Not_found ->
        (let logmsg=Printf.sprintf "lookup_tree: Not_found - Return current element (%s/%s)" ver file in
	Bolt.Logger.log "" Bolt.Level.FATAL logmsg);
	tree
    )

let compute_new_pos_with_gumtree (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  Debug.profile_code_silent "Gumtree.compute_new_pos_with_gumtree"
    (fun () ->
      try
	match List.assoc (ver, file) diffs with
	    Ast_diff.Gumtree root ->
	      begin
		let matched_tree = lookup_tree ver file pos root in
		show_gumtree true 0 matched_tree;
		Bolt.Logger.log "" Bolt.Level.TRACE "-----------------";
		match get_pos_after matched_tree with
		    Some (_, _, bl, bc, el, ec) ->
		      if bl == el then
			(true, (Ast_diff.Sing bl, bc, ec))
		      else
			(true, (Ast_diff.Cpl (bl, el), bc, ec))
		  | None -> (true, (Ast_diff.Deleted false, 0, 0))
	      end
	  | Ast_diff.DeletedFile -> (true, (Ast_diff.Unlink, 0, 0))
	  | _ -> raise (Unexpected "Wrong diff type")
      with Not_found ->
	let msg = "No gumtree diff for "^ file ^ " in vers. " ^ ver in
	Bolt.Logger.log "" Bolt.Level.FATAL msg;
	raise (Unexpected msg)
    )
