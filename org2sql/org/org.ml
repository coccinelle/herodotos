open Lexing

(* let auto_correl = ref 0 *)

exception Unrecoverable

let status = [Ast_org.BUG;
	      Ast_org.FP;
	      Ast_org.UNKNOWN;
	      Ast_org.IGNORED]

let statuslist = String.concat " " (List.map Org_helper.get_status status)
let orgtail = "* org config\n\n#+SEQ_TODO: TODO | "^statuslist^"\n"

let emptyarray vlist =
  Array.init (Array.length vlist) (fun _ -> ([], Hashtbl.create 97))

let line_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.Line _  -> true
		       | _ -> false) ops
  with
      Ast_org.Line l -> l
    | _ -> raise Unrecoverable

let start_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.ColB _ -> true
		       | _ -> false) ops
  with
      Ast_org.ColB b -> b
    | _ -> raise Unrecoverable

let end_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.ColE _ -> true
		       | _ -> false) ops
  with
      Ast_org.ColE e -> e
    | _ -> raise Unrecoverable

let position ops =
  (line_of ops, start_of ops, end_of ops)

let clean_link_text prefix v f pos t =
  let (l,_,_) = pos in
  let endstr = Printf.sprintf "%s/%s::%d" v f l in
  let re = Str.regexp (" ?"^(Str.quote endstr)^"$") in
  let re_prefix = Str.regexp_string prefix in
  let new_t = Str.replace_first re_prefix "" t in
    Str.global_replace re "" new_t

let parse_line file lnum line : Ast_org.org option =
  try
    let lexbuf = Lexing.from_string line in
      try
	Misc.init_line file lnum lexbuf;
	let ast = Org_parser.oneline Org_lexer.token lexbuf in
	  Some ast
      with
	  (Org_lexer.Lexical msg) ->
	    let pos = lexbuf.lex_curr_p in
	      Misc.report_error
		{ Ast.file  = file;
		  Ast.line  = pos.pos_lnum;
		  Ast.colfr = pos.pos_cnum - pos.pos_bol;
		  Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
		("Org Lexer Error: " ^ msg);
	| Org_parser.Error ->
	    let pos = lexbuf.lex_curr_p in
	      Misc.report_error
		{ Ast.file  = file;
		  Ast.line  = pos.pos_lnum;
		  Ast.colfr = pos.pos_cnum - pos.pos_bol;
		  Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
		("Org Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")
  with Sys_error msg ->
    prerr_endline ("*** WARNING *** "^msg);
    None

let re = Str.regexp ("^" ^ Str.quote "#+SEQ_TODO:")

let parse_org v file lnum ch =
  Debug.profile_code_silent "parse_org"
    (fun () ->
       lnum := !lnum + 1;
       let line = input_line ch in
	 if line = "* org config"
	   || line = ""
	   || Str.string_match re line 0
	 then
	   None
	 else
	   (if v then prerr_endline line;
	    parse_line file !lnum line)
    )

let flat_link prefix depth link =
  Debug.profile_code_silent "parse_org"
    (fun () ->
       let (p, ops, t) = link in
       let (ver, file) = Misc.strip_prefix_depth prefix depth p in
       let pos = position ops in
	 (file, ver, pos, t)
    )
