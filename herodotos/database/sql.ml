exception Malformed of string


let s2s s = "'"^Str.global_replace (Str.regexp_string "'") "''" s^"'"

let get_correlation_id = "last_value(correlation_idx)"
let get_new_correlation_id = "nextval('correlation_idx')"

let filter_annot org =
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
let get_report_id prefix cfile cver cpos =
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

let insert_correl_report prefix src patt org =
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

let print_orgs ch prefix orgfile orgs =
  let file = Filename.chop_suffix (Filename.basename orgfile) Global.bugext in
  let (p,patt) =
    (* TODO: Fixme. This may not work according to the names of the projects *)
    match Str.split (Str.regexp_string Global.sep) file with
	[] -> raise (Malformed ("Malformed bug filename: "^orgfile))
      | p::tail -> (p,String.concat Global.sep tail)
  in
    List.iter
      (fun o ->
	 let orgstr = insert_correl_report prefix orgfile patt o in
	   Printf.fprintf ch "%s" orgstr
      ) orgs

(*
 *********************************************
 **          PROCESSING OF NOTES            **
 *********************************************
 *)
let insert_note prefix src patt org =
  let (_, s, r, file, ver, pos, face, t, _, _, sub) = org in
(*  let text = Org.clean_link_text prefix ver file pos t in*)
  let (line, colb, cole) = pos in
  let file_id = "get_file('"^ver^"', '"^Misc.get_canonical_name prefix ver file^"')" in
  let values = String.concat ", "
    [file_id;
     s2s src;
     s2s patt;
     string_of_int line;
     string_of_int colb;
     string_of_int cole;
     s2s t]
  in
    Printf.sprintf "(%s)" values

let print_orgs_as_notes ch prefix orgfile orgs =
  let file = Filename.chop_suffix (Filename.basename orgfile) Global.bugext in
  let (p,patt) =
    (* TODO: Fixme. This may not work according to the names of the projects *)
    match Str.split (Str.regexp_string Global.sep) file with
	[] -> raise (Malformed ("Malformed bug filename: "^orgfile))
      | p::tail -> (p,String.concat Global.sep tail)
  in
  let fields = "file_id, data_source,  note_error_name, line_no, column_start, column_end, text_link" in
  let values = String.concat ",\n\t" (List.map (insert_note prefix orgfile patt) orgs) in
    Printf.fprintf ch "INSERT INTO notes (%s) VALUES\n\t%s;\n" fields values
