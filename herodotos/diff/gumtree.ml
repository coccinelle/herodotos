open Xml

exception MalformedXML
exception Unexpected of string

let diffcmd = "gumtree --output asrc "

(* Get positions in reference tree - With asrc option, it is the source tree *)
(* Get positions in the associated tree - With asrc, it is the destination tree *)
let get_pos inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "pos" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

let get_length inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "length" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

let get_beginline inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "line_before" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

let get_begincol inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "col_before" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

let get_endline inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "line_after" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

let get_endcol inassociated attrs =
  let searchedkey = (if inassociated then "other_" else "") ^ "col_after" in
  let str = snd (List.find (fun (key, value) -> key = searchedkey) attrs) in
  int_of_string str

(**)

let dummy_pos = (0,0,0,0,0,0)
let get_pos inassociated attributes =
  (get_pos inassociated attributes,
   get_length inassociated attributes,
   get_beginline inassociated attributes,
   get_begincol inassociated attributes,
   get_endline inassociated attributes,
   get_endcol inassociated attributes)

let rec parse_tree xml =
  match xml with
      Element("tree", attributes, children) ->
	let pos_before = get_pos false attributes in 
	let pos_after =
	  try
	    Some (get_pos true attributes)
	  with _ -> None
	in 
	Ast_diff.Tree(pos_before, pos_after, List.map parse_tree children)
    | _ ->
      print_endline "parse_tree";
      print_endline (Xml.to_string xml);
      raise MalformedXML

let parse_diff v prefix file =
  if v then print_endline ("Parsing "^file);
  try
    let ver_file = Misc.strip_prefix prefix file in
    let x = open_in_bin file in
    [(ver_file, Ast_diff.Gumtree (input_value x))]
  with Xml.Error _ ->
    let newfile = file^Global.failed in
    if v then print_endline ("Failed while parsing: check "^newfile);
    Sys.rename file newfile;
    []

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
    prerr_int bl;
    prerr_string ":";
    prerr_int bc;
    prerr_string "-";
    prerr_int el;
    prerr_string ":";
    prerr_int ec

let rec show_gumtree dorec depth tree =
  let before = get_pos_before tree in
  let oafter = get_pos_after tree in
  let children = get_children tree in
  prerr_string (String.make depth ' ');
  prerr_string "- ";
  show_gumtree_pos before;
  prerr_string " -> ";
  (match oafter with
      Some after -> show_gumtree_pos after
    | None -> prerr_string "XXX"
  );
  prerr_newline ();  
  if dorec then
    List.iter (show_gumtree dorec (depth +1)) children

(* *)
let is_perfect pos (tree:Ast_diff.tree) =
  let (line, colb, cole) = pos in
  let (_, _, bl, bc, el, ec)  = get_pos_before tree in
  line = bl && line = el
       && colb = bc && cole = ec

let found tree =
  if !Misc.debug then
    (show_gumtree false 0 tree;
     prerr_endline "-----------------";
     true)
  else true
 
let match_tree pos (tree:Ast_diff.tree) =
  let (line, colb, cole) = pos in
  let (_, _, bl, bc, el, ec)  = get_pos_before tree in
  if line > bl && line <= el then
    (* Match a node that start before, but may end at the right line, or after *)
    found tree
  else if line = bl && line < el
       && colb >= bc then
    (* Match a *large* node that start at the right line. The begin column must be right too. *)
    found tree
  else if line = bl && line = el
	       && colb >= bc && cole <= ec then
    (* The perfect line matching is not capture in the previous case.
       We check it here, and also check the columns.
    *)
    found tree
  else
    false

let rec lookup_tree pos (tree:Ast_diff.tree) : Ast_diff.tree =
  Debug.profile_code_silent "Gumtree.lookup_tree"
    (fun () ->
      let children = get_children tree in
      try
	let candidate = List.find (match_tree pos) children in
	if is_perfect pos candidate then
	  candidate
	else
	  lookup_tree pos candidate
      with Not_found ->
	tree
    )

let compute_new_pos_with_gumtree (diffs: Ast_diff.diffs) file ver pos : Ast_diff.lineprediction * int * int =
  Debug.profile_code_silent "Gumtree.compute_new_pos_with_gumtree"
    (fun () ->
      let (line, colb, cole) = pos in
      try
	let root =
	  Debug.profile_code_silent "Gumtree.compute_new_pos#List.assoc"
	  (fun () ->
	    match List.assoc (ver, file) diffs with
		Ast_diff.Gumtree tree -> tree
	      | _ -> raise (Unexpected "Wrong diff type")
	  )
	in
	 let matched_tree = lookup_tree pos root in
	 if !Misc.debug then
	   (show_gumtree true 0 matched_tree;
	    prerr_endline "-----------------");
	 match get_pos_after matched_tree with
	     Some (_, _, bl, bc, el, ec) ->
	       if bl == el then
		 (Ast_diff.Sing bl, bc, ec)
	       else
		 (Ast_diff.Cpl (bl, el), bc, ec)
	   | None -> (Ast_diff.Deleted, 0, 0)
      with Not_found -> (Ast_diff.Sing line, colb, cole)
    )
