open Lexing

(* let auto_correl = ref 0 *)
exception Error of string
exception Unrecoverable

let status = [Ast_org.BUG;
	      Ast_org.FP;
	      Ast_org.UNKNOWN;
	      Ast_org.IGNORED]

let statuslist = String.concat " " (List.map Org_helper.get_status status)


let orgtail = "* org config\n\n#+SEQ_TODO: TODO | "^statuslist^"\n"


let emptyarray vlist =
  Array.init (Array.length vlist) (fun _ -> ([], Hashtbl.create 97))

let make_orglink_of_bug prefix bug =
  let (_, _, _, f, v, (l,b,e), face, t, _, _) = bug in
    (
      prefix^v^"/"^f,
      [Ast_org.Face face;
       Ast_org.Line l;
       Ast_org.ColB b;
       Ast_org.ColE e],
      t ^ Printf.sprintf " %s%s/%s::%d" prefix v f l
    )

let face_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.Face _  -> true
		       | _ -> false) ops
  with
      Ast_org.Face f -> f
    | _ -> raise Unrecoverable


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

(* Prefix should have a trailing '/' *) (*apparamment appelée pour le .new.org (lien_bug) *)
let make_orglinkbug_freetext prefix (bug: Ast_org.bug) =
  let (_, _, _, file, ver, pos, face, t, _, _, _) = bug in
  let (line, cb, ce) = pos in
    Printf.sprintf
      "[[view:%s%s/%s::face=%s::linb=%d::colb=%d::cole=%d][%s]]"
      prefix ver file face line cb ce t

(* Prefix should have a trailing '/' *)  (*apparamment appelée pour le .correl.org (lien_bug) *)
let make_orglinkbug w_freetext prefix bug =
  let (_, _, _, file, ver, pos, face, t, _, _, _) = bug in
  let (line, cb, ce) = pos in
    if w_freetext then
      let clean_t = clean_link_text prefix ver file pos t in
      let new_t = if clean_t = "" then "" else clean_t ^ " " in
	Printf.sprintf
	  "[[view:%s%s/%s::face=%s::linb=%d::colb=%d::cole=%d][%s%s%s/%s::%d]]"
	  prefix ver file face line cb ce new_t prefix ver file line
    else
	Printf.sprintf
	  "[[view:%s%s/%s::face=%s::linb=%d::colb=%d::cole=%d][%s%s/%s::%d]]"
	  prefix ver file face line cb ce prefix ver file line


let rec make_star level =
  if level > 0 then
    if level = max_int then
      ""
    else
      "*" ^ make_star (level -1)
  else
    ""

let make_link prefix link =
  let (p, ops, t) = link in
  let pos = position ops in
  let face = face_of ops in
  let (line, cb, ce) = pos in
      Printf.sprintf "[[view:%s%s::face=%s::linb=%d::colb=%d::cole=%d][%s]]"
	prefix p face line cb ce t


let rec make_org prefix org =
  let Ast_org.Org(l, s, r, link, sub) = org in
  let head = String.concat " "
    (List.filter (fun x -> x <> "")
       [ make_star l ; Org_helper.get_status s ; r])
  in
  let head = Printf.sprintf "%s %s" head (make_link prefix link) in
  let tail = List.map (make_org prefix) sub in
  let tailstring = String.concat "\n" tail in
    head ^ if tailstring = "" then "" else "\n" ^ tailstring


let make_flat_org prefix org =
  let (l, s, r, file, ver, pos, face, t, _, _, sub) = org in
  let head = String.concat " "
    (List.filter (fun x -> x <> "")
       [ make_star l ; Org_helper.get_status s ; r])
  in
  let link = make_orglinkbug_freetext prefix org in
  let head = Printf.sprintf "%s %s" head link in
  let tail = List.map (make_org "") sub in
  let tailstring = String.concat "\n" tail in
    head ^ if tailstring = "" then "" else "\n" ^ tailstring

let parse_line file lnum line : (int * Ast_org.org) option =
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
    LOG "*** WARNING *** %s" msg LEVEL WARN;
    None

let push_stack stack org =
  [org]::stack

let pop_stack stack =
  match stack with
      [] -> []
    | [head] -> [head]
    | head::bighead::tail ->
	match bighead with
	    [] -> raise Unrecoverable
	  | tophead::toptail ->
	      let Ast_org.Org(lvl, s, r, link, sub) = tophead in
		((Ast_org.Org(lvl, s, r, link, sub@(List.rev head)))::toptail)::tail

let upd_head stack org =
  match stack with
      [] -> [[org]]
    | head::tail -> (org::head)::tail

let rec parse_line_opt v file olnum ch =
  let lnum = olnum + 1 in
  let lineopt =
    try
      Some (input_line ch)
    with End_of_file -> None
  in
    match lineopt with
	None -> (lnum, None)
      | Some line ->
(* 	  if !Misc.debug then prerr_string (Printf.sprintf "% 3d " lnum); *)
	  if line = "* org config" then
	    parse_line_opt v file lnum ch
	  else if line = "" then
	    parse_line_opt v file lnum ch
	  else
	    let re = Str.regexp ("^" ^ Str.quote "#+SEQ_TODO:") in
	    if Str.string_match re line 0 then
	      parse_line_opt v file lnum ch
	    else
	      (LOG line LEVEL TRACE;
	       (lnum, parse_line file lnum line))

(*
let dbg_cvt_lvl level =
  if level = max_int then
    "**"
  else
    Printf.sprintf "% 2d" level

let dbg_stack_el prefix stack =
  Misc.print_stack (List.map (fun o -> prefix ^(make_org 0 "" o)) stack)

let dbg_stack clevel level stack =
  Printf.eprintf " C:%s / L:%s\n" (dbg_cvt_lvl clevel) (dbg_cvt_lvl level);
  match stack with
      [] -> ()
    | head::tail ->
	Printf.eprintf "HEAD\n";
	dbg_stack_el "" head;
	Printf.eprintf "TAIL\n";
	List.fold_left (fun i s -> dbg_stack_el (string_of_int i) s;i+1) 1 tail;
	Printf.eprintf "-----------------------\n"
*)

let rec reduce_lines v file lnum (clevel, lineinfo, stack) ch =
  let (level, org) = lineinfo in
(*     if !Misc.debug then dbg_stack clevel level stack; *)
    if clevel = level then
      (* Same level: update stack head, reduce with stack*)
      let newstack = upd_head stack org in
      let (lnum, sinfo, newstack) = parse_lines v file lnum (level, newstack) ch in
	shift_lines v file lnum (clevel, sinfo, newstack) ch
    else if clevel < level then
      (* Sub-level: push in new environment at the beginning, pop at the end *)
      let newstack = push_stack stack org in
      let (lnum, sinfo, newstack) = parse_lines v file lnum (level, newstack) ch in
	shift_lines v file lnum (clevel, sinfo, newstack) ch
    else (* clevel > level *)
      (lnum, Some lineinfo, pop_stack stack) (* Reduce clevel elements *)

and shift_lines v file lnum (clevel, sinfo, stack) ch =
  match sinfo with
      None          -> parse_lines v file lnum (clevel, stack) ch
    | Some lineinfo -> reduce_lines v file lnum (clevel, lineinfo, stack) ch

and parse_lines v file olnum (clevel, stack) ch =
  let (lnum, lineopt) = parse_line_opt v file olnum ch in
    match lineopt with
	None          -> (lnum, None, pop_stack stack)
      | Some lineinfo -> reduce_lines v file lnum (clevel, lineinfo, stack) ch

let parse_all_lines v file ch =
  let (_, rem, orgs) = parse_lines v file 0 (0, []) ch in
    match rem with
	None     -> orgs
      | Some (_,org) -> [org]::orgs

let parse_org v file : Ast_org.orgs =
  Debug.profile_code_silent "parse_org"
    (fun () ->
       try
	 let in_ch = if file <> "-" then open_in file else stdin in
	 let ast = List.flatten (parse_all_lines v file in_ch) in
	   close_in in_ch;
	   ast
       with Sys_error msg ->
	 LOG "*** WARNING *** %s" msg LEVEL WARN;
	 []
    )

let get_string_pos (line, cb, ce) =
  let sline = string_of_int line in
  let scb = string_of_int cb in
  let sce = string_of_int ce in
  let snpos = "("^sline  ^"@"^ scb ^"-"^ sce ^ ")" in
    snpos

let get_string_new_pos diffcheck_pos =
  match diffcheck_pos with
      (Ast_diff.Sing line,colb,cole) ->
	let check_pos = (line, colb, cole) in
	get_string_pos check_pos
    | (Ast_diff.Deleted _, _,_) ->
      "Deleted line"
    | (Ast_diff.Unlink, _,_) ->
      "Deleted file"
    | (Ast_diff.Cpl (lineb,linee),colb, cole) ->
      "Somewhere"

let show_bug verbose bug =
  let (_, _, _, _, ver, pos, _, _,_, _, _) = bug in
  ver ^ get_string_pos pos

let flat_link prefix depth link =
  let (p, ops, t) = link in
      let (ver, file) = Misc.strip_prefix_depth prefix depth p in
      let pos = position ops in
      let face = face_of ops in
(*
      let re = Str.regexp (Str.quote prefix) in
      let new_t = Str.replace_first re "" t in
*)
	(file, ver, pos, face, t)

let extract_link org =
  let Ast_org.Org(lvl, s, r, link, sub) = org in
    link

(* TODO + FIXME --- Should remove duplicate bug reports *)
let find_all_org_w_status org orgs =
  let (_, so, _, fo, vo, po, _, _, _, _, _) = org in
    List.find_all (fun (_, s, _, f, v, p, _, _, _, _, _) ->
		     s = so && f = fo && v = vo && p = po
		  ) orgs

(*
  Used to group similar main entries
  Sub-items are regrouped.
*)

let update_list orglist org =
  let tomerge = find_all_org_w_status org orglist in
    if tomerge = [] then
      org::orglist
    else
      let subtomerge =
	List.flatten (List.map
			(fun (_, _, _, _, _, _, _, _, _, _, sub) -> sub)
			tomerge)
      in
      let newlist = List.filter (fun e -> not (List.mem e tomerge)) orglist in
      let (l, s, r, f, v, pos, face, t, h, n, sub) = org in
	(l, s, r, f, v, pos, face, t, h, n, sub@subtomerge)::newlist

let rec filter_orgs orgs =
  match orgs with
      []       -> []
    | hd::tail ->
	let t2 = filter_orgs tail in
	  update_list t2 hd

let flat_org prefix depth raw_org : Ast_org.bug =
  let Ast_org.Org(lvl, s, r, link, sub) = raw_org in
  let (file, ver, pos, face, t) = flat_link prefix depth link in
    (lvl, s, r, file, ver, pos, face, t, {Ast_org.is_head=true}, {Ast_org.def=None}, sub)

let get_version_name file = let dirList = Str.split (Str.regexp "/") file in
                                   let vname = List.nth dirList (List.length dirList -2) in
                                   (vname,file)


let get_version prefix depth vlist raw_org=
  let org = flat_org prefix depth raw_org in
  let (_, _, _, file, ver, _, _, _, _, _, _) = org in
  Misc.get_idx_of_version vlist ver

let flat_org_for_arrBis  prefix depth (flist,tbl) (raw_org:Ast_org.org)  =
  let org = flat_org prefix depth raw_org in
  let (_, _, _, file, ver, _, _, _, _, _, _) = org in
  let (orglist:Ast_org.bug list) = try Hashtbl.find tbl file with _ -> [] in
  let newlist = update_list orglist org in
    Hashtbl.replace tbl file newlist;
    if not (List.mem file flist) then
       (file::flist, tbl)
    else
       (flist,tbl)

let flat_org_for_arr prefix depth vlist orgarray (raw_org:Ast_org.org) : unit =
  let org = flat_org prefix depth raw_org in
  let (_, _, _, file, ver, _, _, _, _, _, _) = org in
  let idx = Misc.get_idx_of_version vlist ver in
  let ((flist, tbl)) = Array.get orgarray idx in
  let (orglist:Ast_org.bug list) = try Hashtbl.find tbl file with _ -> [] in
  let newlist = update_list orglist org in
    Hashtbl.replace tbl file newlist;
    if not (List.mem file flist) then
      Array.set orgarray idx (file::flist, tbl)

let format_orgs prefix depth orgs =
  Debug.profile_code_silent "format_orgs"
    (fun () -> let flat_orgs_to_filter = List.map (flat_org prefix depth) orgs in
       filter_orgs flat_orgs_to_filter
    )

let build_org_fct prefix depth resultsdir pdir orgfile vers =
  let (vname,i,tm,i2) = vers in
  let file = resultsdir ^ pdir ^ "/" ^ vname ^ "/" ^ orgfile in
  (* if !Misc.debug then Printf.eprintf "Parsing Org file %s\n" file; *)
  let orgs = parse_org false file in
  List.fold_left (fun arrayelt org ->
    flat_org_for_arrBis prefix depth arrayelt org
  ) ([], Hashtbl.create 97) orgs

let build_org_arr prefix depth resultsdir pdir orgfile vlist : Ast_org.orgarray =
  let f = build_org_fct prefix depth resultsdir pdir orgfile in
  Array.of_list (Parmap.parmap f (Parmap.A vlist))
  (* Array.map f vlist *)

let format_orgs_to_arr prefix depth vlist (orgs:Ast_org.orgs ) : Ast_org.orgarray =
   Debug.profile_code_silent "format_orgs_to_arr"
    (fun () -> let orgarray = emptyarray vlist in
       List.iter (flat_org_for_arr prefix depth vlist orgarray) orgs;
       orgarray
    )

let count = ref 0

let show_buglist verbose bugs =
  count := 0;
  List.iter (fun bug ->
    LOG "#%03d %s" !count (show_bug verbose bug) LEVEL TRACE;
    count := !count + 1;
  ) bugs

let show_org verbose prefix (orgs: (string*Ast_org.bugs  list) list) =
  count := 0;
  LOG "SHOW ORG" LEVEL TRACE;
  LOG "Prefix used: %s" prefix LEVEL TRACE;
  List.iter (fun (file, bugslist) ->
    LOG file LEVEL TRACE;
    List.iter (fun bugs ->
      let vers = String.concat " -> "
	(List.map (show_bug verbose) bugs)
      in
      LOG "#%03d in vers. %s" !count vers LEVEL TRACE;
      count := !count + 1
    ) bugslist;
    LOG "" LEVEL TRACE
  ) orgs

let print_orgs_raw ch prefix orgs =
  List.iter
    (fun o ->
       let orgstr = make_flat_org prefix o in
	 Printf.fprintf ch "%s\n" orgstr
    ) orgs;
  Printf.fprintf ch "%s" orgtail (* *)


let print_bugs_raw ch prefix bugs =
  let bug = List.hd bugs in
    Printf.fprintf ch "%s\n" (make_flat_org prefix bug);
    List.iter
      (fun  bug ->
	 Printf.fprintf ch "** %s\n"
	   (make_orglinkbug true prefix bug)
      ) bugs

let print_bugs ch prefix orgs =
  List.iter (fun (_, bugslist) ->
	       List.iter (print_bugs_raw ch prefix) bugslist
	    )
    orgs;
  Printf.fprintf ch "%s" orgtail

let compute_correlation prefix depth correl =
  LOG "compute_correlation: start" LEVEL TRACE;
  try
    let list = List.map (flat_org prefix depth) correl in
    List.fold_left (fun correllist b ->
      let (_, s, _, file, ver, pos, _, t, _, _,sub) = b in
      try
	let l = extract_link (List.hd sub) in
	let new_t = clean_link_text prefix ver file pos t in
	let (nfile, nver, npos, _, _) = flat_link prefix depth l in
	(s, file, ver, pos, nfile, nver, npos, new_t)::correllist
      with Failure msg ->
	LOG "compute_correlation: failure with %s" msg LEVEL FATAL;
	correllist
    ) [] list
  with Misc.Strip msg ->
    LOG "compute_correlation: failure with %s" msg LEVEL FATAL;
    []

let show_correlation verbose correl =
  LOG "SHOW CORRELATION" LEVEL TRACE;
  List.iter (fun b ->
    let (s, file, ver, pos, nfile, nver, next, t) = b in
    LOG "%s %s %s %s -> %s %s %s"
      (Org_helper.get_status s)
      file ver (get_string_pos pos)
      nfile nver (get_string_pos next)
      LEVEL TRACE
  ) correl

let sort_bug bug1 bug2 =
  let (l1, s1, r1, f1, v1, pos1, _, t1, _, _, sub1) = bug1 in
  let (l2, s2, r2, f2, v2, pos2, _, t2, _, _, sub2) = bug2 in
  let dt = compare t1 t2 in
  if dt <> 0 then
    dt
  else
    let df = compare f1 f2 in
    if df <> 0 then
      df
    else
      let dv = compare v1 v2 in
      if dv <> 0 then
	dv
      else
	let dpos = compare pos1 pos2 in
	if dpos <> 0 then
	  dpos
	else
	  compare bug1 bug2

let sort bugarray =
  Array.iter
    (fun (flist, tbl) ->
       List.iter
	 (fun file ->
	    let bugs = Hashtbl.find tbl file in
	      Hashtbl.replace tbl file
		(List.sort sort_bug bugs)
	 ) flist
    ) bugarray

let list_of_bug_array bugarray =
  Array.fold_left
    (fun head (flist, tbl) ->
       head @ List.flatten
	 (List.map
	    (fun file ->
	       Hashtbl.find tbl file
	    ) flist
	 )
    ) [] bugarray

let length bugarray =
  Array.fold_left
    (fun sum (flist, tbl) ->
      sum + (List.fold_left
	       (fun sum file ->
		 sum + (List.length (Hashtbl.find tbl file))
	       ) 0 flist
	 )
    ) 0 bugarray

(* Compare versions *)
let num_version version =
  let vers_format = Str.regexp "[0-9].*" in
  let length_vname = String.length version in
  let posb = Str.search_forward vers_format version 0 in
  let ver_nums = String.sub version posb (length_vname - posb) in
  let numbers = Str.split (Str.regexp "\\.") ver_nums in
  List.map (fun s-> int_of_string s ) numbers

let rec is_more_recent lv1 lv2 =
  match lv1,lv2 with
      [],l2->false
    |l1,[]->true
    |p1::q1,p2::q2 ->begin
      if p1 > p2 then true
      else if p1 < p2 then false
      else (is_more_recent q1 q2)
    end

let is_more_recent_v version1 version2 =
  is_more_recent (num_version version1) (num_version version2)

let is_new bug version =
  let (_,_,_,_,vn,_,_,_,_,_,_) = bug in
  vn = version

(*returns bug's version corresponding to the given version *)
let org_version bug version : Ast_org.org =
  let (_,_,_,_,_,_,_,_,_,_,orgs) = bug in
  let ver = Str.regexp (".*"^version^"/") in
  List.find(fun org-> let Ast_org.Org(_,_,_,link,_)=org in
                      let (path,_,_) = link in
                      Str.string_match ver path 0 )orgs

let rec orgs_version prefix orgs version  =
  (*let ver = Str.regexp (".*"^version^"/") in*)
  match orgs with
      []->[]
    |Ast_org.Org(i,s,n,link,o)::orgstail->
      let (file, ver, pos2, face, t) = flat_link prefix 1 link in
      if(ver = version or (is_more_recent_v ver version)) then
        ((Ast_org.Org(i,s,n,link,o))::orgstail)
      else
        (orgs_version prefix orgstail version)

(*-----------------------------------------------------------------------------------------------------------------------------------------------*)
