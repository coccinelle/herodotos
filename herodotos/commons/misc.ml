open Lexing

exception NoNext of string
exception Strip of string


(* To enable debug mode -- Really verbose *)
let debug = ref false

(* From Devil, thank you Laurent *)
(* **************************************************************** *)

let print_pos pos =
  Printf.sprintf("File \"%s\", line %d, characters %d-%d:\n")
    pos.Ast.file pos.Ast.line pos.Ast.colfr pos.Ast.colto;;

(*
  pos.Ast.file pos.Ast.line pos.Ast.colfr pos.Ast.colto;;
  pos.Lexing.pos_fname pos.Lexing.pos_lnum pos.Lexing.pos_bol 0;;
*)

let getpos pos ofs =
    {
      Ast.file  = pos.pos_fname;
      Ast.line  = pos.pos_lnum;
      Ast.colfr = pos.pos_cnum - pos.pos_bol;
      Ast.colto = ofs - pos.pos_bol;
    }

let dummy_pos = getpos Lexing.dummy_pos 0

let print_error pos str =
  let logmsg=Printf.sprintf "%s" ((print_pos pos) ^ str) in
  Bolt.Logger.log "" Bolt.Level.FATAL logmsg

let report_error pos str =
  print_error pos str;
  exit 2

(* for the parser *)

let init filename lexbuf =
  lexbuf.lex_curr_p <- { pos_fname = filename;
   			 pos_lnum  = 1;
   			 pos_bol   = 1;
   			 pos_cnum  = 1}

let init_line filename lnum lexbuf =
  lexbuf.lex_curr_p <- { pos_fname = filename;
   			 pos_lnum  = lnum;
   			 pos_bol   = 1;
   			 pos_cnum  = 1}

(* **************************************************************** *)

let string_of_char c = String.make 1 c

(* **************************************************************** *)
(*
  Helper to strip a prefix from a absolute path
  Prefix is project dir.
  It returns subdir. as the version name
  and the remaining path as the relative file name.
*)
let strip_prefix prefix path =
  try
    let p2 = Str.replace_first (Str.regexp_string prefix) "" path in
    let re = Str.regexp "^\\([^/]+\\)/\\(.*\\)$" in
    ignore(Str.string_match re p2 0);
    let ver = Str.matched_group 1 p2 in
    let file = Str.matched_group 2 p2 in
    (ver, file)
  with Invalid_argument _ ->
    raise (Strip ("Pb stripping \""^ prefix ^ "\" in \"" ^ path^"\""^
		     "\n\tCheck you have a trailing '/'."))


(*
  Helper to strip a prefix from a absolute path
  Prefix is project dir.
  It returns subdir. as the version name
  and the remaining path as the relative file name.
*)

let rec recompose depth tokens =
  match (depth, tokens) with
      (_, []) -> ("", "")
    | (0,  _) -> ("", String.concat "/" tokens)
    | (d, hd::tail) ->
	let (sub, file) = recompose (d-1) tail in
	  ("/" ^ hd ^ sub, file)

let strip_prefix_depth prefix depth path =
	try
  let p2 = Str.replace_first (Str.regexp_string prefix) "" path in
  let re = Str.regexp "^\\([^/]+\\)/\\(.*\\)$" in
    ignore(Str.string_match re p2 0);
    let ver = Str.matched_group 1 p2 in
    let dirfile = Str.matched_group 2 p2 in
    let tokens = Str.split (Str.regexp_string (Str.quote "/")) dirfile in
    let (sub, file) = recompose (depth-1) tokens in
      (ver ^ sub, file)
      with Invalid_argument _ ->
      	raise (Strip ("Pb stripping \""^ prefix ^ "\" in \"" ^ path^"\""^
		"\n\tCheck you have a trailing '/'."))

(* **************************************************************** *)

(*
  Helper to strip a prefix from a absolute path
  Prefix is projects dir.
  It returns the 1st subdir. as the project name
  then the 2nd subdir as the version name
  and the remaining path as the relative file name.
*)
(*
let strip_prefix_wo_prj prefix path =
	try
  let p2 = Str.replace_first (Str.regexp_string prefix) "" path in
  let re = Str.regexp "^\\([^/]+\\)/\\([^/]+\\)/\\(.*\\)$" in
    ignore(Str.string_match re p2 0);
    let prj = Str.matched_group 1 p2 in
    let ver = Str.matched_group 2 p2 in
    let file = Str.matched_group 3 p2 in
      (prj, ver, file)
      with Invalid_argument _ ->
      	raise (Strip ("Pb stripping "^ prefix ^ " in " ^ path^
		"\n\tCheck you have a trailing '/'."))
*)
(* **************************************************************** *)

(*
  Helpers for version array manipulations
*)
let rec get_idx_of_version_sub vlist v i =
  try
    let (vname,_,_, _) = Array.get vlist i in
    if vname = v then
      i
    else
      get_idx_of_version_sub vlist v (i+1)
  with Invalid_argument _ ->
    raise (NoNext v)

let get_idx_of_version vlist v =
  try
    get_idx_of_version_sub vlist v 0
  with NoNext v ->
    (* *)
    prerr_endline ("No next version found for "^ v);
    Array.iter (fun (x, _, _, _) -> prerr_endline ("V: "^ x)) vlist;
    (*  *)
    -2

let get_version_name vlist idx =
  let (vname, _, _, _) = Array.get vlist idx in
    vname

let get_next_version vlist v =
  try
    let idx = get_idx_of_version vlist v in
      get_version_name vlist (idx+1)
  with Invalid_argument _ -> ""

(* **************************************************************** *)
(*
  Helper to manipulate OCaml time Unix.tm obj
*)
let string_of_date tm =
  let d = tm.Unix.tm_mday in
  let m = tm.Unix.tm_mon in
  let y = tm.Unix.tm_year in
    Printf.sprintf "%02d/%02d/%04d" (m+1) d (y+1900)

let get_year_of tm =
  tm.Unix.tm_year + 1900

(* **************************************************************** *)
(*
  Helper to build a list of unique elements
*)
let unique_list list =
  List.fold_left (fun u e ->
		    if List.mem e u then
		      u
		    else
		      e :: u
		 ) [] list

let filter_opt optlist =
  List.fold_left (fun good opt ->
		    match opt with
			None -> good
		      | Some a -> a::good
		 ) [] optlist

(* **************************************************************** *)
(*
  Create an empty file in the base directory
*)
let touch prefix file =
  let ch = open_out (prefix ^ "/"^file) in
    close_out ch

(*
let rec create_dir verbose dirname =
  try
    let ok = Unix.opendir dirname in
      Unix.closedir ok
  with Unix.Unix_error _ ->
    create_dir verbose (Filename.dirname dirname);
    if verbose then prerr_endline ("Creating directory "^dirname);
    try
      Unix.mkdir dirname 0o755
    with Unix.Unix_error (code, cmd, msg) ->
      prerr_endline ("Failed to create \""^msg^"\" directory.")
*)
let rec create_dir verbose dirname =
  try
    let ok = Unix.opendir dirname in
      Unix.closedir ok
  with Unix.Unix_error _ ->
    if verbose then prerr_endline ("Creating directory "^dirname);
    let status = Unix.system ("mkdir -p "^ dirname) in
      match status with
	  Unix.WEXITED 0 -> ()
	| _         ->
	    prerr_endline ("Failed to create \""^dirname^"\" directory.")

let create_dir_and_open v filename =
  let dirpath = Filename.dirname filename in
  let _ = create_dir v dirpath in
  let outch = open_out filename in
    outch

let get_canonical_name prefix ver filename =
  let base = prefix ^ ver ^ "/" in
  let file = base ^filename in
  let cwd = Sys.getcwd () in
  let dir = Filename.dirname file in
  let filebase = Filename.basename file in
  let cdir = Sys.chdir dir ; Sys.getcwd () in
    Sys.chdir cwd;
    (Str.replace_first (Str.regexp_string base) "" cdir) ^ "/" ^filebase

(* **************************************************************** *)

let string_of_rgb (r,g,b) =
  string_of_float r ^ " " ^ string_of_float g ^" " ^ string_of_float b

let string_of_bool b = if b then "true" else "false"


(* **************************************************************** *)
(*
  To print a list of messages
*)
let print_stack msgstack =
  List.iter (fun msg -> Printf.eprintf "%s\n" msg) msgstack

(* **************************************************************** *)
(*
  Retrieve the number of available cores
  on the system when on a Linux system.
*)
let get_number_of_cores () =
  if Sys.os_type = "Unix" then
    if Sys.file_exists "/proc/cpuinfo" then
      let cmd = "cat /proc/cpuinfo | grep \"processor\" | wc -l" in
      let ch = Unix.open_process_in cmd in
      let cores = int_of_string (input_line ch) in
	match Unix.close_process_in ch with
	    Unix.WEXITED 0 -> cores
	  | _         -> 1
    else
      1
  else
    1

(*
  Retrieve the size of a file
*)
let size_of_file file =
  let ch = open_in file in
  let size = in_channel_length ch in
    close_in ch;
    size

(* **************************************************************** *)
(* last status change time of most recent modified orgfile   *)
(* Author: Manseur *)
let get_change_stat resultsdir pdir vlist orgfile_base acc :float = 
  Array.iter(fun (vname,_,_,_)->acc:=(vname^"/"^orgfile_base)::!acc)vlist;
  let status_change_times =
    List.map(fun f->let file =resultsdir^pdir^"/"^f in 
                    (Unix.stat file).Unix.st_ctime )  (List.rev !acc) in 
  List.hd(List.rev(List.sort compare status_change_times))

