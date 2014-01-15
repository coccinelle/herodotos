
open Printf
open Postgresql

exception Unexpected of string

let sqldate_to_tm sqldate =
  match Str.split (Str.regexp_string "-") sqldate with
      [year;month;day] ->
	let y = int_of_string year in
	let m = int_of_string month in
	let d = int_of_string day in
	  Some (snd (Unix.mktime
		 {Unix.tm_mon=m-1; Unix.tm_mday=d; Unix.tm_year=y-1900;
		  (* Don't care about the time *)
		  Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
		  (* Will be normalized by mktime *)
		  Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
		 }
	      ))
    | _ -> raise (Unexpected "Unsupported format date. YYYY-MM-DD expected from PostgreSQL.")

let print_conn_info conn =
  printf "dbname  = %s\n" conn#db;
  printf "user    = %s\n" conn#user;
  printf "pass    = %s\n" conn#pass;
  printf "host    = %s\n" conn#host;
  printf "port    = %s\n" conn#port;
  printf "tty     = %s\n" conn#tty;
  printf "option  = %s\n" conn#options;
  printf "pid     = %i\n" conn#backend_pid;
  flush stdout

let print_res conn res =
  match res#status with
  | Empty_query -> printf "Empty query\n"
  | Command_ok -> printf "Command ok [%s]\n" res#cmd_status
  | Tuples_ok ->
      printf "Tuples ok\n";
      printf "%i tuples with %i fields\n" res#ntuples res#nfields;
      print_endline (String.concat ";" res#get_fnames_lst);
      for tuple = 0 to res#ntuples - 1 do
        for field = 0 to res#nfields - 1  do
          printf "%s, " (res#getvalue tuple field)
        done;
        print_newline ()
      done
  | Copy_out -> printf "Copy out:\n"; conn#copy_out print_endline
  | Copy_in -> printf "Copy in, not handled!\n"; exit 1
  | Bad_response -> printf "Bad response: %s\n" res#error; conn#reset
  | Nonfatal_error -> printf "Non fatal error: %s\n" res#error
  | Fatal_error -> printf "Fatal error: %s\n" res#error

let print_tuples table =
  let dimx = ArrayLabels.length table in
  let dimy = ArrayLabels.length table.(0) in
  Printf.eprintf "Tuples: %d\n" dimx;
  Printf.eprintf "Fields: %d\n" dimy;
  for tuple = 0 to dimx - 1 do
    for field = 0 to dimy - 1  do
      Printf.eprintf "| %s\t" table.(tuple).(field)
    done;
    Printf.eprintf "\n"
  done

let build_varray res =
  let vs =
    let rec init tuple =
	if tuple = res#ntuples then
	  []
	else
	  let name = res#getvalue tuple 0 in (* Should be version_name *)
	  let date = sqldate_to_tm (res#getvalue tuple 1) in (* Should be release_date *)
	    (name, Some date, 0)::init (tuple+1) (* Size is considered to be zero *)
    in
      init 0
  in
    Config.compute_versinfos vs

(*
  let labellist = res#get_fnames_lst in
  print_endline (String.concat ";" labellist);

  let find_idx_of labellst name =
  let labels = Array.of_list labellst in
  let rec lkup i =
  if (Array.get labels i) = name then
  i
  else
  lkup (i+1)
  in
  lkup 0
*)

let read_tuples vb conn res =
  match res#status with
    | Empty_query -> raise (Unexpected "Empty query")
    | Command_ok -> raise (Unexpected ("Command ok ["^res#cmd_status^"]\n"))
    | Tuples_ok ->
	if vb then printf "%i tuples with %i fields\n" res#ntuples res#nfields;
	let data = Array.init (res#ntuples)
	  (fun tuple -> float_of_string (res#getvalue tuple 2))
	in
	let varray = build_varray res in
	  (varray, data)
    | Copy_out -> raise (Unexpected "Copy out")
    | Copy_in -> raise (Unexpected "Copy in")
    | Bad_response -> raise (Unexpected ("Bad response: "^ res#error))
    | Nonfatal_error -> raise (Unexpected ("Non fatal error: "^ res#error))
    | Fatal_error -> raise (Unexpected ("Fatal error: " ^ res#error))

let read_grtuples vb conn res =
  match res#status with
    | Empty_query -> raise (Unexpected "Empty query")
    | Command_ok -> raise (Unexpected ("Command ok ["^res#cmd_status^"]\n"))
    | Tuples_ok ->
	begin
	  if vb then printf "%i tuples with %i fields\n" res#ntuples res#nfields;
	  match res#get_fnames_lst with
	      xlegend:: ylegend::_ ->
		let data = Array.init (res#ntuples)
		  (fun tuple ->
		     let label = res#getvalue tuple 0 in
		     let value =
		       Array.init 1 (fun _ ->
				       try
					 Some (float_of_string (res#getvalue tuple 1))
				       with _ -> None
				    )
		     in
		       (label, value)
		  )
		in
		  (xlegend, ylegend, Array.to_list data)
	    | _ ->  raise (Unexpected "SQL query must return at least two columns")
	end
    | Copy_out -> raise (Unexpected "Copy out")
    | Copy_in -> raise (Unexpected "Copy in")
    | Bad_response -> raise (Unexpected ("Bad response: "^ res#error))
    | Nonfatal_error -> raise (Unexpected ("Non fatal error: "^ res#error))
    | Fatal_error -> raise (Unexpected ("Fatal error: " ^ res#error))

let dump_single_res conn =
  match conn#get_result with
  | Some res -> print_res conn res; flush stdout
  | None -> ()

let rec dump_res conn =
  match conn#get_result with
  | Some res -> print_res conn res; flush stdout; dump_res conn
  | None -> ()

let dump_tuples vb conn =
  match conn#get_result with
  | Some res -> read_tuples vb conn res
  | None -> raise (Unexpected "No results !")

let dump_grtuples vb conn =
  match conn#get_result with
  | Some res -> read_grtuples vb conn res
  | None -> raise (Unexpected "No results !")

let rec dump_notification conn =
  match conn#notifies with
  | Some (msg, pid) ->
      printf "Notication from backend %i: [%s]\n" pid msg;
      flush stdout;
      dump_notification conn
  | None -> ()

let listener conn =
  try
    while true do
      let socket : Unix.file_descr = Obj.magic conn#socket in
      let _ = Unix.select [socket] [] [] 1. in
      conn#consume_input;
      dump_notification conn
    done
  with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)

let open_db vb conninfo : connection =
  if Obj.is_block (Obj.repr Unix.stdin) then
    failwith "cannot run on Windows";
  let conn = new connection ~conninfo () in
  if vb then print_conn_info conn;
  flush stdout;
  conn#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s);
  ignore(Thread.create listener conn);
  conn

let close_db (conn: connection) =
  conn#finish

let send_query (conn:connection) (sql: string) =
  conn#send_query sql

let get_tuples vb (conn:connection) (sql: string) =
  read_tuples vb conn (conn#exec sql)
(*   conn#send_query sql; *)
(*   dump_tuples conn *)

let get_grtuples vb (conn:connection) (sql: string) =
  read_grtuples vb conn (conn#exec sql)
(*   conn#send_query sql; *)
(*   dump_grtuples conn *)


let get_tuples_of (conn : connection) sqlquery =
  conn#send_query sqlquery;
  match conn#get_result with
      Some res ->
	begin
	  match res#status with
	      Tuples_ok ->
		let dimx = res#ntuples in
		let dimy = res#nfields in
		let table = ArrayLabels.make_matrix ~dimx ~dimy "" in
		for tuple = 0 to dimx - 1 do
		  for field = 0 to dimy - 1  do
		    table.(tuple).(field) <- res#getvalue tuple field
		  done;
		done;
		table
	    | _ -> raise (Unexpected res#error)
	end
    | None -> raise (Unexpected "No result!")

let cmd vb (conn : connection) sqlquery =
  if vb then prerr_endline ("Sending SQL: " ^sqlquery);
  conn#send_query sqlquery;
  dump_single_res conn

let interactive_mode conn =
  try
    while true do
      print_string "> ";
      let s = read_line () in
      send_query conn s;
      dump_res conn
    done
  with End_of_file -> conn#finish


(*************************************************
let main () =
  if Obj.is_block (Obj.repr Unix.stdin) then
    failwith "cannot run on Windows";
  let user = "npalix" in
  let dbname = "atocs" in
  let host = "localhost" in
  let port = "5432" in
  let conn = new connection ~host ~user ~dbname ~port () in
  print_conn_info conn;
  flush stdout;
  conn#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s);
  let _ = Thread.create listener conn in
  try
    while true do
      print_string "> ";
      let s = read_line () in
      conn#send_query s;
      dump_res conn
    done
  with End_of_file -> conn#finish

let test () =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
***************************************************)

let test (conn:connection) =
  try
    interactive_mode conn
  with Error e -> prerr_endline (string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)
