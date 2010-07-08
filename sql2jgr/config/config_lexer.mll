
{

  open Lexing
  module P=Config_parser

  exception Lexical of string

  let tok = Lexing.lexeme
  let commentdepth = ref 0

  let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

  let id_tokens lexbuf = P.TId(lexbuf)

  let extract_string str =  String.sub str 1 ((String.length str) - 2)

  let set_newline lexbuf =
    let curp = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { pos_fname = curp.pos_fname;
   			     pos_lnum  = curp.pos_lnum + 1;
   			     pos_bol   = curp.pos_cnum;
   			     pos_cnum  = curp.pos_cnum}

}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let special = ['-' '.']
let letter  = ['A'-'Z' 'a'-'z' '_']
let dec     = ['0'-'9']
let oct     = ['0'-'7']
let hex     = ['0'-'9' 'a'-'f' 'A'-'F']

let alphanum = (letter | dec)
let salphanum = alphanum (alphanum | special)*

(* let decimal = ('0' | (['1'-'9'] dec*\)) *)
let decimal = dec+
let float = (decimal '.' decimal | '.' decimal)

rule token = parse
  | ['\t']                    {	token lexbuf }
  | ['\n' '\r' '\011' '\012']
      {	set_newline lexbuf; token lexbuf (*P.EOL*) }
  | "//"                      { lcomment lexbuf  }
  | "/*"                      {
      commentdepth := !commentdepth + 1;
      comment lexbuf
			      }
  | "author"                  { P.TAUTHOR        }
  | "color"                   { P.TCOLOR         }
  | "correl"                  { P.TCORREL        }
  | "cpucore"                 { P.TCPUCORE       }
  | "curve"                   { P.TCURVE         }
  | "data"                    { P.TDATA          }
  | "dbconn"                  { P.TDBCONN        }
  | "dir"                     { P.TDIR           }
  | "empty"                   { P.TEMPTY         }
  | "factor"                  { P.TFACTOR        }
  | "file"                    { P.TFILE          }
  | "filename"                { P.TFILENAME      }
  | "flags"                   { P.TFLAGS         }
  | "format"                  { P.TFORMAT        }
  | "graph"                   { P.TGRAPH         }
  | "group"                   { P.TGROUP         }
  | "info"                    { P.TINFO          }
  | "legend"                  { P.TLEGEND        }
  | "linestyle"               { P.TLINESTYLE     }
  | "marktype"                { P.TMARKTYPE      }
  | "marksize"                { P.TMARKSIZE      }
  | "none"                    { P.TNONE          }
  | "nooccurcolor"            { P.TCLEANCOLOR    }
  | "notexistcolor"           { P.TNOTEXISTCOLOR }
  | "occurcolor"              { P.TPATTERNCOLOR  }
  | "pattern"                 { P.TPATTERN       }
  | "prefix"                  { P.TPREFIX        }
  | "project"                 { P.TPROJECT       }
  | "projects"                { P.TPROJECTS      }
  | "prune"                   { P.TPRUNE         }
  | "ratio"                   { P.TRATIO         }
  | "scm"                     { P.TSCM           }
  | "size"                    { P.TSIZE          }
  | "sql"                     { P.TSQL           }
  | "subdir"                  { P.TSUBDIR        }
  | "versions"                { P.TVERSIONS      }
  | "vmin"                    { P.TVMIN          }
  | "website"                 { P.TWEBSITE       }
  | "xaxis"                   { P.TXAXIS         }
  | "xmin"                    { P.TXMIN          }
  | "ymin"                    { P.TYMIN          }
  | "ymax"                    { P.TYMAX          }
  | "xlegend"                 { P.TXLEGEND       }
  | "yaxis"                   { P.TYAXIS         }
  | "ylegend"                 { P.TYLEGEND       }
  | "ylegendfactor"           { P.TYLEGENDFACTOR }
  | "true"                    { P.TBOOL(true)    }
  | "false"                   { P.TBOOL(false)   }
(*  | ":="                      { P.TCOLEQ } *)
  | "::"                      { P.TDCOLON        }
  | ":"                       { P.TCOLON         }
  | '-'                       { P.TMINUS }
  | '+'                       { P.TPLUS  }
  | '*'                       { P.TSTAR  }
  | '/'                       { P.TSLASH }
  | ','                       { P.TCOMMA }
  | ' '                       { token lexbuf }
  | "!="                      { P.TSQLOP(tok lexbuf)   }
  | "<>"                      { P.TSQLOP(tok lexbuf)   }
  | "<="                      { P.TSQLOP(tok lexbuf)   }
  | ">="                      { P.TSQLOP(tok lexbuf)   }
  | "."                       { P.TSQLOP(tok lexbuf)   }
  | '='                       { P.TEQUAL }
  | '('                       { P.TLPAR }
  | ')'                       { P.TRPAR }
  | '{'                       { P.TLCB }
  | '}'                       { P.TRCB }
  | ';'                       { P.TSC }
  | '<'                       { P.TLT }
  | '>'                       { P.TGT }
(*   | '['                       { P.TLAB } *)
(*   | ']'                       { P.TRAB } *)

  | '"'                       { P.TSTRING2Q (string2q lexbuf) }

  | '\''                      { P.TSTRING1Q (string1q lexbuf) }

  | float                     { P.TFloat(float_of_string(tok lexbuf)) }
  | decimal                   { P.TInt(int_of_string(tok lexbuf)) }
  | salphanum*                { id_tokens (tok lexbuf) }
  | eof                       { P.EOF }
  | _ { lexerr "unrecognised symbol, in token rule: " (tok lexbuf) }

and string2q = parse
  | '"'                                       { "" }
  | ['\n' '\r' '\011' '\012'] as x
      {	set_newline lexbuf; Misc.string_of_char x ^ string2q lexbuf (*P.EOL*) }
  | (_ as x)                   { Misc.string_of_char x ^ string2q lexbuf }
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string2q lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string2q lexbuf }
  | ("\\" (_ as v)) as x
       {
         (match v with
         | 'n' -> ()  | 't' -> () | 'v'  -> ()  | 'b' -> () | 'r' -> ()
	 | 'f' -> ()  | 'a' -> ()
	 | '\\' -> () | '?' -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> ()
         | '\n' -> set_newline lexbuf
         | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string2q lexbuf
       }
  | _ { lexerr "unrecognised symbol: " (tok lexbuf) }

and string1q = parse
  | '\''                                       { "" }
  | ['\n' '\r' '\011' '\012'] as x
      {	set_newline lexbuf; Misc.string_of_char x ^ string1q lexbuf (*P.EOL*) }
  | (_ as x)                   { Misc.string_of_char x ^ string1q lexbuf }
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string1q lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string1q lexbuf }
  | ("\\" (_ as v)) as x
       {
         (match v with
         | 'n' -> ()  | 't' -> () | 'v'  -> ()  | 'b' -> () | 'r' -> ()
	 | 'f' -> ()  | 'a' -> ()
	 | '\\' -> () | '?' -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> ()
         | '\n' -> set_newline lexbuf
         | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string1q lexbuf
       }
  | _ { lexerr "unrecognised symbol: " (tok lexbuf) }

and lcomment = parse
  | ['\n' '\r' '\011' '\012']
      {	set_newline lexbuf; token lexbuf (*P.EOL*) }
  | eof                       { P.EOF }
  | _ { lcomment lexbuf}

and comment = parse
    "/*"                      {
      commentdepth := !commentdepth + 1;
      comment lexbuf
			      }
  | "*/"                      {
      commentdepth := !commentdepth - 1;
      if !commentdepth = 0 then
	token lexbuf
      else
	comment lexbuf
			      }
  | ['\n' '\r' '\011' '\012']
      {	set_newline lexbuf; comment lexbuf (*P.EOL*) }
  | eof                       { P.EOF }
  | _ { comment lexbuf}
