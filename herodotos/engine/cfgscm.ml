
open Printf

let create_tmp_tbl = "create table tmp_bug_authors (
correlation_id       int            primary key,
commit_id            VarChar(64)    not null,  
author_id            int    
);"
let init_bugs =  "select correlation_id, version_name, file_name, line_no from full_reports r where release_date = correlation_birth_date and status = 'BUG';"
let tmp_update = "update correlations c set birth_commit_number = commit_id, author_id = tmp.author_id
from tmp_bug_authors tmp where c.correlation_id = tmp.correlation_id;"

let tmp_insert = format_of_string "insert into tmp_bug_authors values %s;"
let tmp_insert_snippet = format_of_string "(%s, get_author_id('%s'), '%s')"

let sql_sanitized string = Str.global_replace (Str.regexp "'") "''" string

let retrieve_author vb conn path vmin correlation =
  let version = correlation.(1) in
  let file = correlation.(2) in
  let line = int_of_string correlation.(3) in
  let (author, sha1) = Git.blame vb path vmin version file line in
  sprintf tmp_insert_snippet correlation.(0) (sql_sanitized author) sha1

let retrieve_authors v1 v2 conn path vmin table =
  let authors =
    Array.to_list (ArrayLabels.map (retrieve_author v2 conn path vmin) table)
  in
  let query = sprintf tmp_insert (String.concat ", " authors) in
  Database.cmd v2 conn query;
  Database.cmd v1 conn tmp_update
  (* Database.print_tuples table *)

let retrieve_correlated_bugs v1 v2 conn (prjname:string) =
  let scmpath = Helper.get_scmpath true prjname in
  if scmpath <> "" then
    begin
      prerr_endline ("Project: " ^ prjname);
      let vlist = snd(Config.get_versinfos prjname) in
      let (vmin,_,_,_) = Array.get vlist 0 in
      (* prerr_endline ("Looking in " ^ scmpath);
	 Git.load_authors scmpath; *)
      let table = Database.get_tuples_of conn init_bugs in
      retrieve_authors v1 v2 conn scmpath vmin table
    end

let blame v1 v2 v3 configfile filter =
  ignore(Config.parse_config configfile);
  [%info_log "Config parsing OK!"];
  Config.show_config ();
  try
    let logmsg=Printf.sprintf "Connecting to %s" !Setup.dbconn in
    [%info_log logmsg];
    let conn = Database.open_db v1 !Setup.dbconn in
    [%debug_log "Connection - OK !"];
    if filter = "" then
      Setup.PrjTbl.iter (fun p _ -> retrieve_correlated_bugs v1 v2 conn p) Setup.projects
    else retrieve_correlated_bugs v1 v2 conn filter;
    [%debug_log "Disconnecting..."];
    Database.close_db conn;
    [%debug_log "Done."]
  with exp ->
    [%error_log "Connection - KO !"];
    let logmsg2=Printf.sprintf "%s" (Printexc.to_string exp) in
    [%error_log logmsg2]
