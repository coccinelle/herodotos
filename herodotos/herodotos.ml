
exception Misconfigured

let configfile = ref ""
let help = ref false
let longhelp = ref false
let orgfile = ref ""
let prefix = ref ""
let version = ref false
let verbose1 = ref false
let verbose2 = ref false
let verbose3 = ref false
let cvs = ref false
let png = ref false
let pdf = ref true
let web = ref false

let init = ref false
let correl = ref false
let stat = ref false
let statcorrel = ref false
let statfp = ref false
let test = ref false
let erase = ref false

let sql = ref false

let freearg = ref ""

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [-c <configurationfile> [--init | --correl | --erase]]\n"

let options = [
  "-c", Arg.Set_string configfile, "file Configuration file describing the requested data";
  "--config", Arg.Set_string configfile, "file Configuration file describing the requested data";
  "--correl", Arg.Set correl, " Correlation mode with the configuration file";
  "--cvs", Arg.Set cvs, " Generation of .cvsignore files (in init mode)";
  "--debug", Arg.Set Misc.debug, " Debug mode";
  "--erase", Arg.Set erase, " Erase some data";
  "-h", Arg.Set help, " Display this list of options";
  "-help", Arg.Set help, " Display this list of options";
  "--help", Arg.Set help, " Display this list of options";
  "--init", Arg.Set init, " Initialize a tracking environment as defined in the configuration file";
  "--longhelp", Arg.Set longhelp, " Display this list of options and the supported graph types";
  "--parse_org", Arg.Set_string orgfile, "file path to an Org file to parse (test)";
  "--prefix", Arg.Set_string prefix, "path prefix of the source directories (test)";
  "--profile", Arg.Unit (fun () -> prerr_endline "*** PROFILING ENABLED ***";
			   Debug.profile := Debug.PALL), " gather timing information about the main functions";
  "--eps", Arg.Clear pdf, " disable the (default) generation of PDF with 'epstopdf'";
  "--png", Arg.Set png, " enable the generation of png images (in default mode)";

  "--to-sql", Arg.Set sql, " Convert the parsed Org file to SQL";

  "--stat", Arg.Set stat, " Compute statistics";
  "--statcorrel", Arg.Set statcorrel, " Compute statistics about correlations";
  "--statfp", Arg.Set statfp, " Compute statistics about false positives";
  "--test", Arg.Set test, " Test for development (test)";
  "--version", Arg.Set version, " Print Herodotos version";
  "-v", Arg.Set verbose1, " verbose mode";
  "-vv", Arg.Set verbose2, " more verbose mode";
  "-vvv", Arg.Set verbose3, " more more verbose mode";
  "-w", Arg.Set web, " generation of the website (in default mode)";
  "--web", Arg.Set web, " generation of the website (in default mode)"
]

(*
let rec get_n s n bugs =
  match bugs with
      [] -> ([],[])
    | hd::tail ->
	if n > 0 then
	  let (t1, t2) = get_n s (n-1) tail in
	    (hd::t1, t2)
	else
	  let (t1, t2) = get_n s s bugs in
	    ([], t1::t2)

let split_n n bugs =
  let (rm, list) = get_n n n bugs in
    rm::list

let multi_output verbose vlist fileexist bfl =
  if !jgrfile = "" then
    raise Misconfigured
  else
    let n_bfl = split_n !jgrsize bfl in
      ignore(List.fold_left (fun n fbl ->
			       let fn = !jgrfile^"."^string_of_int n^".jgr" in
			       let outch = open_out fn in
			       let xaxis  = "version" in
			       let xlabel = "Versions" in
			       let ylabel = "Bugs" in
			       let notexistcolor = "0 0 0" in
			       let cleancolor    = "1 1 1" in
			       let defectcolor   = "0 1 1" in
			       let bugs = Helper.compute_graph verbose false fileexist fbl vlist "" None in
				 print_endline fn;
				 Defects.draw_graph (verbose,false, false) outch vlist bugs xaxis xlabel ylabel notexistcolor cleancolor defectcolor None;
				 close_out outch;
				 n+1
			    ) 0 n_bfl)
*)

let main aligned =
  if !version then
    print_endline ("Herodotos version "^ Global.version)
  else
    if !test then
      Test.test !configfile
    else
      begin
	if !verbose3 then verbose2 := true;
	if !verbose2 then verbose1 := true;
	if (!help || !longhelp) then
	  (
	    Arg.usage aligned usage_msg;

	    if !longhelp then
	      prerr_endline ("\n"^Cfgmode.supported_types^"\n")
	  )
	else
	  let anystat = !stat || !statfp || !statcorrel in
	    if ((String.length !configfile) <> 0) then
	      if !init && not !correl && not anystat && not !erase then
		Debug.profile_code "initialize env."
		  (fun () -> Cfginit.init_env !verbose1 !verbose2 !verbose3 !configfile !cvs)
	      else if not !init && !correl && not anystat && not !erase  then
		Debug.profile_code "correlation"
		  (fun () -> Cfgcorrel.correl !verbose1 !verbose2 !verbose3 !configfile !freearg)
	      else if not !init && not !correl && anystat && not !erase then
		Debug.profile_code "statistics"
		  (fun () -> Cfgstat.stats !verbose1 !verbose2 !verbose3 !configfile !freearg !statcorrel !statfp)
	      else if not !init && not !correl && not anystat && !erase then
		Debug.profile_code "erase env."
		  (fun () -> Cfgerase.erase_env !verbose1 !verbose2 !verbose3 !configfile !freearg)
	      else
		(
		  print_endline ("Herodotos version "^ Global.version);
		  prerr_endline ("Processing "^ !configfile);
		  Cfgmode.graph_gen !verbose1 !verbose2 !verbose3 !configfile !pdf !png !web !freearg;
		  prerr_newline ()
		)

	    else
	      (* For Org file parsing *)
	      (* For converting Org file to SQL entries *)
	      if ((String.length !orgfile) <> 0)
	      then
		begin
		  if ((String.length !prefix) = 0) then prerr_endline "*** WARNING *** Prefix not set";
		  if not !sql then prerr_endline " Parsing...\n";
		  let ast = Org.parse_org (not !sql) !orgfile in
		    if not !sql then prerr_endline ("\nChecking... ("^string_of_int (List.length ast)^" elements)");
		    if !Misc.debug then
		      (Misc.print_stack (List.map (Org.make_org "") ast);
		       prerr_newline ();
		      );
		    let (msg, formatted) =
		      try
			("", Org.format_orgs !prefix 1 ast)
		      with Misc.Strip msg -> (msg, [])
		    in
		      if not !sql then
			prerr_endline ("\nConverting... ("^string_of_int (List.length formatted)^" elements)\n");
 		      if msg <> "" then prerr_endline msg;
		      if formatted = []
		      then prerr_endline "Failed!"
		      else
			(if !sql then
			   Sql.print_orgs stdout !prefix !orgfile formatted
			 else
			   Org.print_orgs_raw stderr !prefix formatted;
			 if not !sql then prerr_endline "\nDone!")
		end

	      else
		Arg.usage aligned usage_msg
      end

let _ =
  let aligned = Arg.align options in
    (try
      Arg.parse_argv Sys.argv aligned (fun x -> freearg := x) usage_msg;
    with Arg.Bad msg ->
      (prerr_string msg; exit 0));
    main aligned;
    prerr_endline (Debug.profile_diagnostic ())


(* For ratio computation *)
(*
  if ((String.length !existfile) <> 0)
  && ((String.length !difffile) <> 0)
  && ((String.length !prefix) <> 0)
  && !ratio then
  begin
  prerr_endline "Display ratio information";
  let diffs = (Diff.parse_diff !prefix !difffile) in
  let ast_exist = Exists.parse_exist !existfile in
  Ratio.print diffs ast_exist
  end
  else
*)
