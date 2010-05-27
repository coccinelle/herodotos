open Helper

let order (_,v1) (_,v2) =
  compare v2 v1

let show_stat verbose vlist bugs file =
  let fn = affected_files bugs in
  let bn = List.length bugs in
    print_endline ("Working on; "^file);
    print_string ((string_of_int fn)^ "; file(s) affected by; ");
    print_endline ((string_of_int bn)^ "; bug(s)");
    if bn <> 0 then
      begin
	let (create, delete) = cd_ratio vlist bugs in
	let (shortest, avgver, longest) = sl_ratio_ver bugs in
	let (shortestday, avgday, longestday) = sl_ratio_day vlist bugs in
	  (if verbose then
	     verbose_stat vlist bugs; print_newline ()
	  );
	  Printf.printf "%4.2f" (float_of_int bn/. float_of_int fn);
	  print_endline "; bug(s) per file";
	  print_int create;
	  print_string "; bug(s) (";
	  print_int (create * 100 / bn);
	  print_endline "%) introduced by a new file.";
	  print_int delete;
	  print_string "; bug(s) (";
	  print_int (delete * 100 / bn);
	  print_endline "%) removed by a file deletion.";
	  print_int shortest;
	  print_endline "; version(s) for the shortest bug life.";
	  print_int longest;
	  print_endline "; version(s) for the longest bug life.";
	  Printf.printf "%4.2f" avgver;
	  print_endline "; version(s) in average for a bug life.";

	  print_int shortestday;
	  print_endline "; days for the shortest bug life.";
	  print_int longestday;
	  print_endline "; days for the longest bug life.";
	  print_int avgday;
	  print_endline "; days in average for a bug life.";
	  List.iter (fun (dir, v) ->
		       Printf.printf "%4d ; " v;
		       Printf.printf "%2.2f %%" (float_of_int (v*100) /. float_of_int bn);
		       if ((String.compare dir "") == 0) then
			 print_endline " ; root"
		       else
			 (print_string " ; ";
			  print_endline dir
			 )
		    ) (List.sort order (compute_bydir bugs));
	  print_newline ()
      end
