exception Malformed of string


let s2s s = "'"^Str.global_replace (Str.regexp_string "'") "''" s^"'"

let get_correlation_id = "last_value(correlation_idx)"
let get_new_correlation_id = "nextval('correlation_idx')"

let filter_annot org : bool =
  let Ast_org.Org(l, _, _, _, _) = org in
    l = max_int

(*
-- describe a position of a note
create table report_annotations (
	report_id            int          not null references reports on delete cascade,
	line_no              int          not null,
	column_start         int          not null,
	column_end           int          not null,
	text_link            VarChar(256)            -- text hyperlink
);
*)
let get_report_id prefix cfile cver (cpos : Ast_org.pos) =
  let (line, colb, cole) = cpos in
  let file_id = "get_file('"^cver^"', '"^Misc.get_canonical_name prefix cver cfile^"')" in
    "(SELECT report_id FROM reports, correlation_idx WHERE file_id="^file_id^
      " AND correlation_id="^get_correlation_id^
      " AND line_no="^string_of_int line^
      " AND column_start="^string_of_int colb^
      " AND column_end="^string_of_int cole^
      ")"

let insert_annot prefix cfile cver cpos org =
  let Ast_org.Org(l, s, r, link, sub) = org in
    if l = max_int then
      let (file, ver, pos, face, t) = Org.flat_link prefix 1 link in
      let text = Org.clean_link_text prefix ver file pos t in
      let (line, colb, cole) = pos in
      let values = String.concat ", "
	[get_report_id prefix cfile cver cpos;
	 string_of_int line;
	 string_of_int colb;
	 string_of_int cole;
	 s2s text]
      in
	Printf.sprintf
	  "INSERT INTO report_annotations SELECT %s;\n" values
    else
      raise (Malformed "*** ERROR *** insert_annot can not process a report.")

(*
-- describe one report
create table reports (
	report_id            serial       primary key,
	correlation_id       int          not null references correlations on delete cascade,
	file_id              int          not null references files on delete cascade,
	line_no              int          not null,
	column_start         int          not null,
	column_end           int          not null,
	text_link            VarChar(256) ,         -- text hyperlink

	unique (report_id, file_id, line_no, column_start)
);
*)


(* remplacera la fonction qu'elle précéde *)
let insert_report_orgs prefix bug version = 
  let (_,_,_,path,vname,pos1,_,text,_,n,orgs) = bug in
  let (line1, colb1, cole1) = pos1 in
  let orgs_interest = Org.orgs_version prefix orgs version in
  let reqs = ref "" in
    List.iter(fun o ->let Ast_org.Org(l, s, r, link, sub) = o in
      let (file, ver, pos2, face, t) = Org.flat_link prefix 1 link in
      let text = Org.clean_link_text prefix ver file pos2 t in
      let (line2, colb2, cole2) = pos2 in
      let file_id = "get_file('"^ver^"', '"^Misc.get_canonical_name prefix ver path^"')" in
      let file_id_for_correl = "get_file('"^vname^"', '"^ Misc.get_canonical_name prefix vname file^"')" in
      let fields = "correlation_id, file_id, line_no, column_start, column_end, text_link" in
      let correlation_id = "get_corr_id("^file_id_for_correl^","^string_of_int line1^","^string_of_int colb1^")" in
      let values = String.concat ", " 
       [correlation_id;
        file_id;
        string_of_int line2;
        string_of_int colb2;
        string_of_int cole2;
        s2s text]
      in
       reqs := !reqs ^ Printf.sprintf "INSERT INTO reports (%s)\n\tSELECT %s FROM correlation_idx;\n" fields values) orgs_interest; 
      !reqs

let insert_report prefix org =
  let Ast_org.Org(l, s, r, link, sub) = org in
  let (file, ver, pos, face, t) = Org.flat_link prefix 1 link in
  let text = Org.clean_link_text prefix ver file pos t in
    if l < max_int then
      let (line, colb, cole) = pos in
      let fields = "correlation_id, file_id, line_no, column_start, column_end, text_link" in
      let file_id = "get_file('"^ver^"', '"^Misc.get_canonical_name prefix ver file^"')" in
      let values = String.concat ", "
	[get_correlation_id;
	 file_id;
	 string_of_int line;
	 string_of_int colb;
	 string_of_int cole;
	 s2s text]
      in
	Printf.sprintf "INSERT INTO reports (%s)\n\tSELECT %s FROM correlation_idx;\n" fields values
    else
      raise (Malformed "*** ERROR *** insert_report can not process an annotation.")

(*
create sequence correlation_idx;

-- describe a set of correlated reports
create table correlations (
	correlation_id       int            primary key,
	report_error_name    VarChar(256)   not null,
	status               VarChar(256)   not null,
	reason_phrase        VarChar(256),           -- annotation
	data_source          VarChar(256)   not null,
	commit_number        VarChar(64)             -- hash of the commit that introduces the bug
);
*)

let insert_correl_report prefix src patt (org:Ast_org.bug) =
  let (_, s, r, file, ver, pos, face, t, _, _, sub) = org in
(*  let text = Org.clean_link_text prefix ver file pos t in*)
  let fields = "correlation_id,  report_error_name, status, reason_phrase, data_source" in
  let status = Org_helper.get_status s in
  let values = String.concat ", " [get_new_correlation_id; s2s patt; s2s status; s2s r; s2s src] in
  let (annots, reports) = List.partition filter_annot sub in
  let annotselt = List.map (insert_annot prefix file ver pos) annots in
  let reportselt = List.map (insert_report prefix) reports in
    "BEGIN;SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;\n"^
      (Printf.sprintf "INSERT INTO correlations (%s)\n\tSELECT %s FROM correlation_idx;\n" fields values) ^
      (String.concat "" reportselt)^
      (String.concat "" annotselt)^
      "COMMIT;\n"

let print_orgs ch prefix orgfile (orgs:Ast_org.bug list) version =
  let basefile = Filename.basename orgfile in
  let file = Filename.chop_suffix basefile Global.bugext in
  let (p,patt) =
    (* TODO: Fixme. This may not work according to the names of the projects *)
    match Str.split (Str.regexp_string Global.sep) file with
	[] -> raise (Malformed ("Malformed Org bug filename: "^orgfile))
      | p::tail -> (p,String.concat Global.sep tail)
  in
    List.iter
      (fun o ->
	 let (_,_,_,_,vname,_,_,_,_,_,orgs) = o in 
	 let orgstr =if ((Org.is_new o version) || (Org.is_more_recent_v vname version ) )then
                       insert_correl_report prefix basefile patt o
                     else
                       try
                         let _ = Org.orgs_version prefix orgs version in
                         insert_report_orgs prefix o version
                           
                       with _->""  
                     in
	   Printf.fprintf ch "%s" orgstr
      ) orgs

(*
 *********************************************
 **          PROCESSING OF NOTES            **
 *********************************************
 *)
let insert_note prefix src patt org =
  let (_, s, r, file, ver, pos, face, t, _, _, sub) = org in
  let text = Org.clean_link_text prefix ver file pos t in
  let (line, colb, cole) = pos in
  let file_id = "get_file('"^ver^"', '"^Misc.get_canonical_name prefix ver file^"')" in
  let values = String.concat ", "
    [file_id;
     s2s src;
     s2s patt;
     string_of_int line;
     string_of_int colb;
     string_of_int cole;
     s2s text]
  in
    Printf.sprintf "(%s)" values

let print_orgs_as_notes ch prefix orgfile orgs =
  let basefile = Filename.basename orgfile in
  let file = Filename.chop_suffix basefile Global.origext in
  let (p,patt) =
    (* TODO: Fixme. This may not work according to the names of the projects *)
    match Str.split (Str.regexp_string Global.sep) file with
	[] -> raise (Malformed ("Malformed Org filename: "^orgfile))
      | p::tail -> (p,String.concat Global.sep tail)
  in
  let fields = "file_id, data_source,  note_error_name, line_no, column_start, column_end, text_link" in
  let values = String.concat ",\n\t" (List.map (insert_note prefix basefile patt) orgs) in
    Printf.fprintf ch "INSERT INTO notes (%s) VALUES\n\t%s;\n" fields values
