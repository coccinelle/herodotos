
exception BadFormat of string

let todo = ref 0

let convert_status orgstatus =
  match orgstatus with
      Ast_org.EMPTY      -> Ast_bugs.Other ""
    | Ast_org.TODO       -> todo := !todo +1 ; Ast_bugs.TODO
    | Ast_org.OK         -> Ast_bugs.OK
    | Ast_org.BUG        -> Ast_bugs.BUG
    | Ast_org.FP         -> Ast_bugs.FP
    | Ast_org.UNKNOWN    -> Ast_bugs.Other "UNKNOWN"
    | Ast_org.IGNORED    -> Ast_bugs.Other "IGNORED"
    | Ast_org.SAME       -> raise (BadFormat "org file containing bugs must use TODO/BUG/FP. SAME not allowed.")
    | Ast_org.UNRELATED  -> raise (BadFormat "org file containing bugs must use TODO/BUG/FP. UNRELATED not allowed.")
    | Ast_org.Unknow (s) -> prerr_endline ("Warning: unknown status type \""^s^"\" in org file containing bugs. Assuming FP!");
	Ast_bugs.FP

let rec convert_opt2line options =
  match options with
      [] -> raise (BadFormat "view org links must have a line position")
    | opt::tail ->
	match opt with
	    Ast_org.Line line -> line
	  | _ -> convert_opt2line tail

let convert_opt2colb options =
  match options with
      [] -> raise (BadFormat "view org links must have a begin column position")
    | opt::tail ->
	match opt with
	    Ast_org.ColB col -> col
	  | _ -> convert_opt2line tail

let convert_opt2cole options =
  match options with
      [] -> raise (BadFormat "view org links must have an end column position")
    | opt::tail ->
	match opt with
	    Ast_org.ColE col -> col
	  | _ -> convert_opt2line tail

let convert_opt2pos file options =
  (convert_opt2line options,
   convert_opt2colb options,
   convert_opt2cole options)

(*
  {
    Ast.file  = file;
    Ast.line  = convert_opt2line options;
    Ast.colfr = convert_opt2colb options;
    Ast.colto = convert_opt2cole options;
  }
*)

let convert_link2version prefix depth (path, _, _) : Ast_bugs.version =
  let (vers, _) = Misc.strip_prefix_depth prefix depth path in
    vers

let convert_org2version prefix depth org : Ast_bugs.version =
  let Ast_org.Org(_, status, _, link, subs) = org in
    convert_link2version prefix depth link

let convert_path2file prefix depth path =
  let (_, file) = Misc.strip_prefix_depth prefix depth path in
    file

let rec convert_org prefix depth org =
  let Ast_org.Org(level, status, reason, link, subs) = org in
  let (path, options, text) = link in
  let file = convert_path2file prefix depth path in
    (level,
     convert_status status,
     reason,
     file,
     text,
     List.map (convert_org2version prefix depth) subs,
     convert_opt2pos file options (* Misc.dummy_pos *)
    )

let convert prefix depth (orgbugs: Ast_org.orgs) : Ast_bugs.bugs =
  Bugs.filter_bugs (List.map (convert_org prefix depth) orgbugs)
