open Xml

exception MalformedXML
exception Unexpected of string

let freearg = ref ""

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ "\n\nOptions:\n"

let options = [
  "-h", Arg.Unit (fun _ -> ()) , " Display this list of options";
  "-help", Arg.Unit (fun _ -> ()), " Display this list of options";
  "--help", Arg.Unit (fun _ -> ()), " Display this list of options"
]

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

let parse_in in_chan =
  let x = Xml.parse_in in_chan in
  parse_tree x
  
let anon_fun = fun x -> freearg := x

let _ =
  let aligned = Arg.align options in
  (try
     Arg.parse_argv Sys.argv aligned anon_fun usage_msg;
   with Arg.Bad msg ->
     (prerr_string msg; exit 0));
  let gumdiff = parse_in stdin  in
  let out = open_out_bin !freearg in
  output_value out gumdiff;
  close_out out
