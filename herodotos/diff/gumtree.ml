open Xml

exception MalformedXML

let diffcmd = "gumtree "

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

let parse_position xml =
  match xml with
      Element("position", attributes, children) ->
	get_pos attributes
    | _ -> raise MalformedXML  

let get_before children =
  let elem = List.find (fun xml ->
    match xml with
	Element("before", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Element(_, _, positions) ->
	(match positions with
	    [position] -> parse_position position
	  | _ -> raise MalformedXML)
    | _ -> raise MalformedXML

let get_after children =
  let elem = List.find (fun xml ->
    match xml with
	Element("after", _, _) -> true
      | _ -> false
  ) children in
  match elem with
      Element(_, _, positions) ->
	(match positions with
	    [position] -> parse_position position
	  | _ -> raise MalformedXML)
    | _ -> raise MalformedXML

let parse_action xml =
  match xml with
      Element("action", attributes, children) ->
	(
	  match get_type attributes with
	      "Insert" -> Ast_diff.Insert (get_after children)
	    | "Move"   -> Ast_diff.Move (get_before children, get_after children)
	    | "Delete" -> Ast_diff.Delete (get_pos attributes)
	    | _ -> raise MalformedXML
	)
    | _ -> raise MalformedXML

let parse_actions xml =
  match xml with
      Element("actions", attributes, children) ->
	List.map parse_action children
    | _ -> raise MalformedXML

let parse_diff v prefix file =
  print_endline file;
  let x = Xml.parse_file file in
  print_endline (Xml.to_string x);
  []

let compute_new_pos_with_gumtree (diffs: Ast_diff.diffs) file ver pos : Ast_diff.lineprediction * int * int =
  Debug.profile_code_silent "Diff.compute_new_pos_with_findhunk"
    (fun () ->
      let (line, colb, cole) = pos in
      (* TODO Implement with gumtree information *)
      (Ast_diff.Sing line, colb, cole)
    )
