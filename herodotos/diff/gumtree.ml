open Xml

exception MalformedXML

let diffcmd = "gumtree "

let get_beginline attrs = 
  let str = snd (List.find (fun (key, value) -> key = "line") attrs) in
  int_of_string str

let get_begincol attrs =
  let str = snd (List.find (fun (key, value) -> key = "line") attrs) in
  int_of_string str
    
let get_endline attrs =
  let str = snd (List.find (fun (key, value) -> key = "line") attrs) in
  int_of_string str

let get_endcol attrs =
  let str = snd (List.find (fun (key, value) -> key = "line") attrs) in
  int_of_string str

let parse_position xml =
  match xml with
      Element("position", attributes, children) ->
	(get_beginline attributes)
    | _ -> raise MalformedXML  

let get_before children =
  let Element(_, _, positions) = List.find (fun xml ->
    match xml with
	Element("before", _, _) -> true
      | _ -> false
  ) children in
  match positions with
      [position] -> parse_position position
    | _ -> raise MalformedXML

let get_after children =
  let Element(_, _, positions) = List.find (fun xml ->
    match xml with
	Element("after", _, _) -> true
      | _ -> false
  ) children in
  match positions with
      [position] -> parse_position position
    | _ -> raise MalformedXML

let parse_edition xml =
  match xml with
      Element("edition", attributes, children) ->
	(get_before children, get_after children)
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
