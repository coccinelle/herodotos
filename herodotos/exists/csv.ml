open Lexing

exception BadFormat
exception Misconfiguration of string

let parse_csv file =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    (* ignore(Parsing.set_trace true); *)
    Misc.init file lexbuf;
    let ast = Exist_parser.entries Exist_lexer.token lexbuf in
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
	    ("CSV Lexer Error: " ^ msg);
	  raise BadFormat
    | Exist_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.print_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum + 1;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("CSV Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'");
	  raise BadFormat


let int_of_state s =
  match s with
    Ast_exist.Empty  -> raise BadFormat
  | Ast_exist.True   -> raise BadFormat
  | Ast_exist.False  -> raise BadFormat
  | Ast_exist.Size i -> i

