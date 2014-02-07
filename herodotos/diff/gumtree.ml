open Xml

exception MalformedXML
exception Unexpected of string

let diffcmd = "gumtree --output xml "

let get_type attrs =
  snd (List.find (fun (key, value) -> key = "type") attrs)

let get_beginline attrs =
  let str = snd (List.find (fun (key, value) -> key = "begin_line") attrs) in
  int_of_string str

let get_begincol attrs =
  let str = snd (List.find (fun (key, value) -> key = "begin_col") attrs) in
  int_of_string str

let get_endline attrs =
  let str = snd (List.find (fun (key, value) -> key = "end_line") attrs) in
  int_of_string str

let get_endcol attrs =
  let str = snd (List.find (fun (key, value) -> key = "end_col") attrs) in
  int_of_string str

let get_pos attributes =
  (get_beginline attributes,
   get_begincol attributes,
   get_endline attributes,
   get_endcol attributes)

(*
let parse_position xml =
  match xml with
      Element("position", attributes, children) ->
	get_pos attributes
    | _ ->
      print_endline "position";
      print_endline (Xml.to_string xml);
      raise MalformedXML
*)

let get_before children =
  let elem = List.find (fun xml ->
    match xml with
	Element("before", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Element(_, attrs, positions) ->
	get_pos attrs
(*
  (match positions with
	    [position] -> parse_position position
	  | _ ->
	    print_endline "before";
	    print_endline (Xml.to_string elem);
	    raise MalformedXML)
*)
    | _ -> raise MalformedXML

let get_after children =
  let elem = List.find (fun xml ->
    match xml with
	Element("after", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Element(_, attrs, positions) ->
	get_pos attrs
(*
  (match positions with
	    [position] -> parse_position position
	  | _ ->
	    print_endline "after";
	    print_endline (Xml.to_string elem);
	    raise MalformedXML)
*)
    | _ -> raise MalformedXML

let parse_action xml =
  match xml with
      Element("action", attributes, children) ->
	(
	  match get_type attributes with
	      "Insert" -> Ast_diff.Insert (get_before children, get_after children)
	    | "Move"   -> Ast_diff.Move (get_before children, get_after children)
	    | "Update"   -> Ast_diff.Update (get_before children, get_after children)
	    | "Delete" -> Ast_diff.Delete (get_before children)
	    | _ ->
	      print_endline "parse_action";
	      print_endline (Xml.to_string xml);
	      raise MalformedXML
	)
    | _ ->
      print_endline "parse_action";
      print_endline (Xml.to_string xml);
      raise MalformedXML

let parse_actions xml =
  match xml with
      Element("actions", attributes, children) ->
	List.map parse_action children
    | _ ->
      print_endline "parse_actions";
      print_endline (Xml.to_string xml);
      raise MalformedXML

let parse_diff v prefix file =
  print_endline file;
  let x = Xml.parse_file file in
  let fff = "v1/foo/" in
  let ver_file = Misc.strip_prefix prefix fff in
  print_endline (Xml.to_string x);
  [(ver_file, Ast_diff.Gumtree (parse_actions x))]

let get_pos_before action =
  match action with
      Ast_diff.Empty -> (0,0,0,0)
    | Ast_diff.Insert (pb, _) -> pb
    | Ast_diff.Move (pb, _) -> pb
    | Ast_diff.Update (pb, _) -> pb
    | Ast_diff.Delete pb -> pb

(* To be used when the fault is NOT inside the modified code *)
let get_offset_after action = (* Offset is given in term of (line, column) *)
  match action with
      Ast_diff.Empty -> (0,0)
    | Ast_diff.Insert (pb, pa) ->
      let (bl1, bc1, el1, ec1) = pb in
      let (bl2, bc2, el2, ec2) = pa in
      (el2 - el1, ec2 - ec1)
    | Ast_diff.Move (pb, pa) ->
      let (bl1, bc1, el1, ec1) = pb in
      let (bl2, bc2, el2, ec2) = pa in
      (el2 - el1, ec2 - ec1)
    | Ast_diff.Update (pb, pa) ->
      let (bl1, bc1, el1, ec1) = pb in
      let (bl2, bc2, el2, ec2) = pa in
      (el2 - el1, ec2 - ec1)
    | Ast_diff.Delete pb ->
      let (bl1, bc1, el1, ec1) = pb in
      (el1 - bl1, ec1 - bc1) (* TODO: Tester l'encode de gumtree pour une ligne supprimée *)

let compute_new_pos_with_gumtree (diffs: Ast_diff.diffs) file ver pos : Ast_diff.lineprediction * int * int =
  Debug.profile_code_silent "Gumtree.compute_new_pos_with_gumtree"
    (fun () ->
      let (line, colb, cole) = pos in
      try
	let actions =
	  Debug.profile_code_silent "Gumtree.compute_new_pos#List.assoc"
	  (fun () ->
	    match List.assoc (ver, file) diffs with
		Ast_diff.Gumtree actions -> actions
	      | _ -> raise (Unexpected "Wrong diff type")
	  )
	in
	 let action = List.fold_left
	   (fun a1 a2 ->
	     let (bl1, bc1, el1, ec1) = get_pos_before a1 in
	     let (bl2, bc2, el2, ec2) = get_pos_before a2 in
		 if bl1 <= line then
		   if line < bl2
		     || line == bl2 && cole < bc2 then
		     a1
		   else
		     a2
		 else
		   a2
	   ) Ast_diff.Empty actions
	 in
	 (*
	   1. Check the action is before the fault
	   2.a. yes, use the implied offset to predict in n+1
	   2.b. no, use action to predict in n+1 (fault is inside the change)
	 *)
	 let (bl, bc, el, ec) = get_pos_before action in
	 if el < line || el == line && ec < colb then
	   let (line_offset, col_offset) = get_offset_after action in
	   (Ast_diff.Sing (line + line_offset), colb + col_offset, cole + col_offset)
	 else
	   match action with
	     Ast_diff.Empty ->
	       (Ast_diff.Sing line, colb, cole)

	   | Ast_diff.Insert _ ->
	     raise (Unexpected "Fault is inside an insert section!")

	   | Ast_diff.Update (pb, pa) ->
	     let (bl, bc, el, ec) = pa in
	     if bl == el then
	       (Ast_diff.Sing line, bc, ec)
	     else
	       (Ast_diff.Cpl (bl, el), bc, ec)

	   | Ast_diff.Move (pb, pa) ->
	     (* begin/end * line/column * before/after *)
	     let (blb, bcb, elb, ecb) = pb in
	     let (bla, bca, ela, eca) = pa in
	     (Ast_diff.Sing (line + bla - blb), colb + bca - bcb, cole + eca - ecb)

	   | Ast_diff.Delete pb ->
	     (Ast_diff.Deleted, 0, 0)
      with Not_found -> (Ast_diff.Sing line, colb, cole)
    )
