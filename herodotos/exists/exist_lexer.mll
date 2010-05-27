
{

  open Lexing
  module P=Exist_parser


  exception Lexical of string
  let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let alphanum = ['A'-'Z' 'a'-'z' '_' '/' '-' '.' '0'-'9']
let num = ['0'-'9']

rule token = parse
  | ['\n' '\r' '\011' '\012']
      {
	let curp = lexbuf.lex_curr_p in
	  lexbuf.lex_curr_p <- { pos_fname = curp.pos_fname;
   				 pos_lnum  = curp.pos_lnum + 1;
   				 pos_bol   = curp.pos_cnum;
   				 pos_cnum  = curp.pos_cnum};
	  P.EOL
      }
  | "true"                    { P.TTrue }
  | "false"                   { P.TFalse }
  | ';'                       { P.TSC }

  | num (num)*                { P.TInt (int_of_string (Lexing.lexeme lexbuf))}
  | alphanum ( alphanum )*    { P.TId(Lexing.lexeme lexbuf) }

  | eof            { P.EOF }
  | _ { lexerr "unrecognised symbol, in token rule: " (Lexing.lexeme lexbuf) }
