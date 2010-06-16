
exception MalformedVersionFile

let build_date strdate =
  match Str.split (Str.regexp "/") strdate with
      [month;day;year] ->
	let m = int_of_string month in
	let d = int_of_string day in
	let y = int_of_string year in
	  snd (Unix.mktime
		 {Unix.tm_mon=m-1; Unix.tm_mday=d; Unix.tm_year=y-1900;
		  (* Don't care about the time *)
		  Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
		  (* Will be normalized by mktime *)
		  Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
		 })
    | _ -> raise MalformedVersionFile

let rec build_item ch =
  try
    let line = input_line ch in
    let vinfo = match Str.split (Str.regexp ";") line with
	[vname;daynum;day;size] -> (vname, int_of_string daynum, build_date day, int_of_string size)
      | _ -> raise MalformedVersionFile
    in
      vinfo::build_item ch
  with End_of_file -> []

let build_vlist vlistfile =
  let in_ch = open_in vlistfile in
  let list = build_item in_ch in
    close_in in_ch;
    Array.of_list list
