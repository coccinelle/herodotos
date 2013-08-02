(*

let difffmt  = format_of_string "git --git-dir %s diff v1..v2 file"
let logfmt   = format_of_string "git --git-dir %s log v1..v2 file"
let headfmt  = format_of_string "git --git-dir %s log -1"

*)

exception Failed
exception MalformedGitLog

let blamefmt = format_of_string "git --git-dir %s blame --incremental %s -L %d,+1 -- %s"
let authorsfmt = format_of_string "git --git-dir %s log --date=short --pretty=format:\"%%an;%%ae;%%ad;%%H\" > %s"

let authordb = Hashtbl.create 5

let is_author line =
  Str.string_match (Str.regexp "^author ") line 0

let is_filename line =
  Str.string_match (Str.regexp "^filename ") line 0

let rec get_first_author ch =
  let line = input_line ch in
    if is_author line then
	Str.replace_first (Str.regexp_string "author ") "" line
    else
      if not (is_filename line) then
	get_first_author ch
      else
	failwith "author not found"

let blame vb vlist (path:string) (idx:int) (line:int) (file:string) =
  let (version,_,_,_) = Array.get vlist idx in
  let cmd = Printf.sprintf blamefmt path version line file in
    if vb then prerr_string ("Executing: "^cmd);
    let ch = Unix.open_process_in cmd in
      ignore(input_line ch); (* Ignore SHA1 and line info *)
      let author = get_first_author ch in
	match Unix.close_process_in ch with
	    Unix.WEXITED 0 ->
	      if vb then prerr_endline (" - OK"); author
	  | _         ->
	      if vb then prerr_endline (" - KO"); raise Failed

let print_duration days =
  if days < 30 then
    string_of_int days ^ " day(s)"
  else if days < 365 then
    string_of_float (ceil (float_of_int days /. 30.5)) ^ " month(s)"
  else
    string_of_float (ceil (float_of_int days /. 365.24)) ^ " year(s)"

let sec_to_days sec =
  int_of_float (sec /. (86400.0)) (* convert secondes into days*)

let tm_to_days tm =
  sec_to_days (fst (Unix.mktime (Config.get_date tm)))

let get_day strdate =
  match Str.split (Str.regexp "-") strdate with
      [year;month;day] ->
	let m = int_of_string month in
	let d = int_of_string day in
	let y = int_of_string year in
	let tm = Unix.mktime
	  {Unix.tm_mon=m-1; Unix.tm_mday=d; Unix.tm_year=y-1900;
	   (* Don't care about the time *)
	   Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
	   (* Will be normalized by mktime *)
	   Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
	  }
	in sec_to_days (fst tm)
    | _ -> raise MalformedGitLog

let authors = ref (Hashtbl.create 1)
let update_author name c_email now =
  try
    let ((_, until) as emaildate, since, countlist) = Hashtbl.find !authors name in
    let (email, _) as newemaildate = if until < now then (c_email, now) else emaildate in
    let newdate = if since < now then since else now in
      Hashtbl.replace !authors name (newemaildate, newdate, now::countlist)
  with _ ->
      Hashtbl.add !authors name ((c_email, now), now, [now])

let count = ref 0

let read_log line =
  try
    let tokens = Str.split (Str.regexp ";") line in
      match tokens with
	  [name;email;date;_ (* commit SHA1 *)] ->
	    update_author name email date;
	    Some true
	| [email;date;_ (* commit SHA1 *)] ->
	    update_author email email date;
	    Some true
	| _ ->
	    prerr_endline ("Malformed entry (retry): skip line "^ string_of_int !count);
	    prerr_endline line;
	    Some false
  with _ -> None

(*
  let res = try Some (get_authors()) with End_of_file -> None in
  match res with .

  mets le try autor du input_line

*)

let read_line ch =
  try
    let line = input_line ch in
      Some line
  with End_of_file -> None

let rec get_authors ch retry =
  match read_line ch with
      None -> ()
    | Some line ->
	count := !count +1;
	match read_log line with
	    Some _ -> get_authors ch retry
	  | None ->
	      prerr_endline ("*** ERROR *** line "^ string_of_int !count);
	      prerr_endline line;
	      Gc.print_stat stderr;
	      prerr_endline ("*** CLEANING ***");
	      Gc.compact ();
	      Gc.print_stat stderr;
	      if retry then
		(
		  prerr_endline ("*** RETRY *** line "^ string_of_int !count);
		  match read_log line with
		      Some success -> get_authors ch success
		    | _ -> ()
		)


let cache_log path =
  let tmp = Filename.temp_file "git-log." ".cache" in
  let cmd = Printf.sprintf authorsfmt path tmp in
    prerr_string ("Retrieving commit info");
    match Unix.system cmd with
	Unix.WEXITED 0 -> prerr_endline (" - OK"); tmp
      | _         -> prerr_endline (" - KO"); raise Failed

let load_authors path =
  try
    if path <> "" then authors := Hashtbl.find authordb path
  with Not_found ->
    authors := Hashtbl.create 541;
    let tmp = cache_log path in
      prerr_string ("Analyzing authors: "^tmp);
      let inch = open_in tmp in
	count := 0;
	try
	  get_authors inch true;
	  close_in inch;
	  Unix.unlink tmp;
	  prerr_endline " - OK";
	  Hashtbl.add authordb path (!authors)
	with MalformedGitLog ->
	  prerr_endline " - KO"

(* Nico: FIXME: Hardcoded date value for Linux git *)
let author_info vlist versionidx name =
  let (_, _, refdate, _) = Array.get vlist versionidx in
  let ((email,until), since, dates)= Hashtbl.find !authors name in
  let sinceday = get_day since in
  let refday = tm_to_days refdate in
  let timespan = refday - sinceday +1 in
(*    prerr_endline (name ^": "^(string_of_int (List.length dates))); *)
   (name ^ " <" ^email ^">", since, until,
     0, dates, "", 0, timespan)
(*
  let totaltimespan = get_day until - sinceday +1 in
  let origtimespan = refday - get_day "2005-04-17" +1 in
  let duration = print_duration timespan in
  let absexpertpct = (100 * timespan) / origtimespan in
    (name ^ " <" ^email ^">", since, until,
     totaltimespan, count, duration, absexpertpct, timespan)
*)

let count_patches name vlist idx dates =
  let (_, _, refdate, _) = Array.get vlist idx in
  let refday = tm_to_days refdate in
  let sum = List.fold_left (fun sum date ->
		    let day = get_day date in
		      if refday > day then
			sum
		      else
			sum +1
		 ) 0 dates
  in
 (*   prerr_endline (name ^": "^string_of_int sum ^" patches before buggy one"); *)
    sum

let prerr_author vlist name version =
  let idx = Misc.get_idx_of_version vlist version in
  let (contact, since, until, totaltimespan, dates, duration, abspct, reldays) =
    author_info vlist idx name in
    prerr_string (contact^": active since "^since^" until "^until);
    prerr_string (" i.e. "^ string_of_int totaltimespan);
    prerr_string (" patches:"^string_of_int (List.length dates));
    prerr_newline ();
    prerr_endline ("Experience at that time: " ^ duration ^
		     " ("^ string_of_int abspct ^"% abs)"^
		     " ("^ string_of_int reldays ^" rel. days)")
