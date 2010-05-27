open Lexing

exception BadFormat
exception Misconfiguration of string

let parse_exist file =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    (* ignore(Parsing.set_trace true); *)
    Misc.init file lexbuf;
    let ast = Exist_parser.main Exist_lexer.token lexbuf in
      close_in in_ch;
      ast
  with
      (Exist_lexer.Lexical msg) ->
	let pos = lexbuf.lex_curr_p in
	  Misc.print_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum + 1;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Exist Lexer Error: " ^ msg);
	  raise BadFormat
    | Exist_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.print_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum + 1;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Exist Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'");
	  raise BadFormat

let analyze_file prjdir file vers =
  (* FIXME HACK for OSDI 2010 *)
  let newfile = Str.replace_first (Str.regexp "^staging/") "drivers/staging/" file in
  let absfile = prjdir ^ "/" ^ vers  ^ "/" ^ newfile in
    if Sys.file_exists absfile then
      Ast_exist.Size (Misc.size_of_file absfile)
    else
      Ast_exist.False

let analyze_vers prjdir verslist file =
  (file,
   List.map (analyze_file prjdir file) verslist
  )

let analyze_rep prjdir verslist filelist =
  (verslist,
   List.map (analyze_vers prjdir verslist) filelist
  )

let min_list l = List.fold_left min 0 l
let max_list l = List.fold_left max 0 l

let min_exist l : (int * bool) =
  List.fold_left
    (fun (i,e) s ->
       if (e || (not e && s)) then
	 (i, true)
       else
	 (i+1, false)
    ) (0, false) l

let max_exist l i : (int * bool) =
  List.fold_right
    (fun s (i,e) ->
       if (e || (not e && s)) then
	 (i, true)
       else
	 (i-1, false)
    ) l (i, false)

let show_ver vlist vs =
  print_string "Analysis from version ";
  print_string (Array.get vlist (min_list vs));
  print_string " to version ";
  print_string (Array.get vlist (max_list vs));
  print_newline ()

let convert_state s =
  match s with
      Ast_exist.Size 0 -> false
    | Ast_exist.False  -> false
    | Ast_exist.Size _ -> true
    | Ast_exist.True   -> true

let compute_entry i e =
  let (f, sl) = e in
  let nsl = List.map convert_state sl in
  let vmin = fst (min_exist nsl  ) in
  let vmax = fst (max_exist nsl i) in
    if vmin < 0 || vmax < 0 then
      raise (Misconfiguration
	       ("\tUnable to compute file existence information for "^f^".\n" ^
		  "\tIs the source code accessible or your cache files uptodate?"))
    else
      (f, vmin, vmax)

let show_entry (f, min, max) =
  print_string "From ";
  print_string (string_of_int min);
  print_string " to ";
  print_string (string_of_int max);
  print_string " ";
  print_string f;
  print_string " exists";
  print_newline ()

let compute_exist vlist ast =
  let (versions, entries) = ast in
  let maxidx = Array.length vlist - 1 in
    (maxidx, List.map (compute_entry maxidx) entries)

let print_to existfilepath ast =
  let ch = open_out existfilepath in
  let (versions, entries) = ast in
    output_string ch "file;";
    List.iter (fun v -> output_string ch (v^";")) versions;
    output_string ch "\n";
    List.iter (fun (f, states) ->
		 output_string ch (f^";");
		 let toks = List.map (fun s ->
			      match s with
				  Ast_exist.True ->
				    "true"
				| Ast_exist.False ->
				    "false"
				| Ast_exist.Size (i) ->
				    string_of_int i
				     ) states in
		   output_string ch (String.concat ";" toks);
		   output_string ch "\n"
	      ) entries;
    close_out ch
