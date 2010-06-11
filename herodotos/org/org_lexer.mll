
{

  open Lexing
  module P=Org_parser

  exception Lexical of string
  let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

  let id_tokens lexbuf =
      match lexbuf with
	| "view"      -> P.TVIEW
	| "face"      -> P.TFACE
	| "linb"      -> P.TLINB
	| "colb"      -> P.TCOLB
	| "cole"      -> P.TCOLE
	| "org"       -> P.TORG
	| "config"    -> P.TCONFIG
	| "TODO"      -> P.TTODO
	| "OK"        -> P.TOK
	| "BUG"       -> P.TBUG
	| "NONBUG"    -> P.TNONBUG
	| "FP"        -> P.TFP
	| "SAME"      -> P.TSAME
	| "UNRELATED" -> P.TUNRELATED
	| "UNKNOWN"   -> P.TUNKNOWN
 	| "IGNORED"   -> P.TIGNORED
	| "SEQ_TODO"  -> P.TSTODO
	| _           -> P.TId(lexbuf)

}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let special = ['-' '.' ',']
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

let alphanum = (letter | digit)
let salphanum = (letter | digit | special)

let linktext = '[' [^']']* ']'

let dec = ['0'-'9']

let decimal = ('0' | (['1'-'9'] dec*))

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
  | ' '                       { P.TSPACE     }
  | '*'                       { P.TSTAR      }
  | ':'                       { P.TCOLON     }
  | '='                       { P.TEQUAL     }
  | '|'                       { P.TVERT      }
  | "#+"                      { P.THASHPLUS  }
  | '['                       { P.TLAB       }
  | ']'                       { P.TRAB       }
  | '/'                       { P.TSLASH     }
  | decimal                   { P.TInt(int_of_string(Lexing.lexeme lexbuf)) }
  | salphanum*                { id_tokens (Lexing.lexeme lexbuf) }
(*  | eof                       { P.EOF } *)
  | _                         { P.TTEXT(Lexing.lexeme lexbuf) }
(*  | _ { lexerr "unrecognised symbol, in token rule: " (Lexing.lexeme lexbuf) } *)
