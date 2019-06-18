open Global

exception Misconfigured

let configfile = ref "study.hc"
let help = ref false
let longhelp = ref false
let diffalgo = ref "gnudiff"
let orgfile = ref ""
let outfile = ref ""
let prefix = ref ""
let extract = ref ""
let version_incr = ref ""
let version = ref false
let verbose1 = ref false
let verbose2 = ref false
let verbose3 = ref false
let cvs = ref false
let png = ref false
let pdf = ref true
let web = ref false

let mode = ref None

let sql = ref false
let sqlnotes = ref false
let sql_update = ref false

let withsizes = ref false

let freearg = ref ""

let usage_msg_headline =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [-c <configurationfile> [preinit | init | correl | graph | erase | blame]]\n\nModes:\n"

let modes = [
  "preinit", Arg.Unit (fun () -> mode := Some PreInit), " Recover missing parts of a version description (size and release date) , or extract version code";
  "init", Arg.Unit (fun () -> mode := Some Init), " Initialize a tracking environment as defined in the configuration file";
  "correl", Arg.Unit ( fun () -> mode := Some Correl), " Correlation mode with the configuration file";
  "graph", Arg.Unit (fun () -> mode := Some Graph), " Generate the graphs";
  "blame", Arg.Unit (fun () -> mode := Some Blame), " Annotate reports with author name";
  "export-history", Arg.Unit (fun () -> mode := Some ExpHistory), " Export scm history to SQL DB (currently date and author of commits)";
  "export-reports", Arg.Unit (fun () -> mode := Some ExpReports), " Export reports to SQL DB (the correlated reports from the new.org files)";
  "extract", Arg.Unit (fun () -> mode := Some Extract), " Extract a <tag> version from a correlated report";
  "stat", Arg.Unit (fun () -> mode := Some Stat), " Compute statistics";
  "statcorrel", Arg.Unit (fun () -> mode := Some Statcorrel), " Compute statistics about correlations";
  "statfp", Arg.Unit (fun () -> mode := Some StatFP), " Compute statistics about false positives";
  "erase", Arg.Unit (fun () -> mode := Some Erase), " Erase some data";
  "test", Arg.Unit (fun () -> mode := Some Test), " Test for development (test)";
  "help", Arg.Unit (fun () -> mode := Some Help), " Display this list of options";
 ]

let usage_msg = Arg.usage_string (Arg.align modes) usage_msg_headline ^ "\n\nOptions:\n"

let options = [

  "-h", Arg.Unit (fun () -> mode := Some Help) , " Display this list of options";
  "-help", Arg.Unit (fun () -> mode := Some Help), " Display this list of options";
  "--help", Arg.Unit (fun () -> mode := Some Help), " Display this list of options";
  "--longhelp", Arg.Unit (fun () -> mode := Some Longhelp), " Display this list of options and the supported graph types";
  "--version", Arg.Unit (fun () -> mode := Some Version), " Print Herodotos version";

  "-c", Arg.Set_string configfile, "file Configuration file describing the requested data";
  "--config", Arg.Set_string configfile, "file Configuration file describing the requested data";
  "--cvs", Arg.Set cvs, " Generation of .cvsignore files (in init mode)";
  "--debug", Arg.Set Misc.debug, " Debug mode";
  "--diff", Arg.Set_string diffalgo, " Diff algorithm (e.g. 'gnudiff' or 'gumtree:file.xml')";
  "--tag", Arg.Set_string extract, "version Gives the version to extract from a correlated report";
  "--hacks", Arg.Set Global.hacks, " Enable hacks (to perform customized studies)";
  "--orgfile", Arg.Set_string orgfile, "file path to an Org file";
  "-o", Arg.Set_string outfile, "file path to an output file";
  "--outfile", Arg.Set_string outfile, "file path to an output file";
  "--prefix", Arg.Set_string prefix, "path prefix of the source directories (to properly parse Org files)";
  "--profile", Arg.Unit (fun () -> LOG "*** PROFILING ENABLED ***" LEVEL TRACE;
			   Debug.profile := Debug.PALL), " gather timing information about the main functions";
  "--eps", Arg.Clear pdf, " disable the (default) generation of PDF with 'epstopdf'";
  "--png", Arg.Set png, " enable the generation of png images (in default mode)";

  "--to-sql", Arg.Set sql, " Convert the parsed Org file to SQL (tables of errors)";
  "--to-sql-notes", Arg.Unit (fun () -> sqlnotes:=true;sql:=true), " Convert the parsed Org file to SQL (table of notes)";
  "--to-sql-update",Arg.Tuple[Arg.Set sql_update;Arg.Set_string version_incr], " Update the database including new versions study results";

  "-v", Arg.Set verbose1, " verbose mode";
  "-vv", Arg.Set verbose2, " more verbose mode";
  "-vvv", Arg.Set verbose3, " more more verbose mode";
  "-w", Arg.Set web, " generation of the website (in default mode)";
  "--web", Arg.Set web, " generation of the website (in default mode)";
  "+s", Arg.Set withsizes, "enable the compute size of linux versions";
  "-s", Arg.Clear withsizes, "disable the compute size of linux versions"
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
  match !mode with
      Some running_mode ->
	begin
	  match running_mode with
	      Version ->
		print_endline ("Herodotos version "^ Global.version);
		LOG "CPU: %d" (Parmap.get_default_ncores ()) LEVEL DEBUG
	    | Help | Longhelp ->
	      Arg.usage aligned usage_msg;
	      
	      if running_mode = Longhelp then
		prerr_endline ("\n"^Cfgmode.supported_types^"\n")
	    | _ -> 
	      if ((String.length !configfile) <> 0) then
		begin
		  if !verbose3 then verbose2 := true;
		  if !verbose2 then verbose1 := true;
		  match running_mode with
		      Test -> Test.test !configfile !diffalgo
		    | Stat | Statcorrel | StatFP -> 
		      Debug.profile_code "statistics"
			(fun () -> Cfgstat.stats !verbose1 !verbose2 !verbose3 !configfile !freearg running_mode)
		    | PreInit ->
		      Debug.profile_code "pre-initialize env."
			(fun () -> Cfgpreinit.preinit !verbose1 !verbose2 !verbose3 !configfile !withsizes)
		    | Init->
		      Debug.profile_code "initialize env."
			(fun () -> Cfginit.init_env !verbose1 !verbose2 !verbose3 !configfile !cvs)
		    | Correl ->
		      Debug.profile_code "correlation"
			(fun () -> Cfgcorrel.correl !verbose1 !verbose2 !verbose3 !configfile !diffalgo !freearg)
		    | Extract ->
		      Debug.profile_code "extract"
			(fun () -> 
			  if ((String.length !orgfile) <> 0)
			  then (
			    if ((String.length !prefix) <> 0) then (
			      let ast = Org.parse_org !verbose1 !orgfile in
			      if ast = [] then
				LOG "Empty Org file: %s" !orgfile LEVEL WARN
			      else
				begin
				  LOG "Checking %d elements..." (List.length ast) LEVEL INFO;
				  let formatted =
				    try
				      Org.format_orgs !prefix 1 ast
				    with Misc.Strip msg ->
				      LOG "Error: %s" msg LEVEL FATAL;
				      failwith msg
				  in
				  LOG "Converting %d elements..."  (List.length formatted) LEVEL INFO;
				  if formatted = [] then
				    LOG "Conversion failed!" LEVEL ERROR
				  else
				    let filtered =
				      if !extract = "" then
					(LOG "No extraction to perform" LEVEL INFO;
					 formatted)
				      else
					(LOG "Extracting version tagged '%s'" !extract LEVEL INFO;
					 Orgfilter.filter_version !extract !prefix formatted)
				    in
				    try
				      let out = if !outfile = "" then stdout else open_out !outfile in
				      Org.print_orgs_raw out !prefix filtered;
				      if !outfile <> "" then close_out out;
				      LOG "Done!" LEVEL INFO
				    with _ -> LOG "Fail to write to %s" !outfile LEVEL ERROR;
				end
			    ) else (
			      LOG "*** ERROR *** Prefix not set" LEVEL ERROR;
			      failwith "No prefix set"
			    )
			  ) else (
			    LOG "*** ERROR *** Org file not set" LEVEL ERROR;
			    failwith "No org file set"
			  )
			)
		    | Graph ->
		      Debug.profile_code "graph generation"
			(fun () ->
			  LOG "Herodotos version %s" Global.version LEVEL INFO;
			  LOG "Processing %s" !configfile LEVEL INFO;
			  Cfgmode.graph_gen !verbose1 !verbose2 !verbose3 !configfile !pdf !png !web !freearg;
			)
		    | Erase ->
		      Debug.profile_code "erase env."
			(fun () -> Cfgerase.erase_env !verbose1 !verbose2 !verbose3 !configfile !freearg)
		    | Blame ->
		      Debug.profile_code "blame authors"
			(fun () -> Cfgscm.blame !verbose1 !verbose2 !verbose3 !configfile !freearg)
		    | ExpHistory ->
		      Debug.profile_code "export scm history"
			( fun () -> () )
			(* (fun () -> Cfgscm.history !verbose1 !verbose2 !verbose3 !configfile !freearg) *)
		    | ExpReports ->
		      Debug.profile_code "export reports"
			( fun () ->
			  (* For Org file parsing *)
			  (* For converting Org file to SQL entries *)
			  if ((String.length !orgfile) <> 0) then
			    begin
			      if ((String.length !prefix) = 0) then LOG "*** WARNING *** Prefix not set" LEVEL WARN;
			      LOG "Parsing...\n" LEVEL INFO;
			      let ast = Org.parse_org false !orgfile in
			      if ast = [] then
				LOG "Empty Org file" LEVEL WARN
			      else
				begin
				  LOG "Checking... (%d elements)" (List.length ast) LEVEL INFO;
				  if !Misc.debug then
				    (Misc.print_stack (List.map (Org.make_org "") ast);
				     prerr_newline ()
				    );
				  let (msg, formatted) =
				    try
				      ("", Org.format_orgs !prefix 1 ast)
				    with Misc.Strip msg -> (msg, [])
				  in
				  LOG "Converting... (%d elements)" (List.length formatted) LEVEL INFO;
 				  if msg <> "" then LOG "%s" msg LEVEL ERROR;
				  if formatted = [] then
				    LOG "Failed!" LEVEL FATAL
				  else
				    (if !sql then
					Sql.print_orgs stdout !prefix !orgfile formatted
				     else if !sqlnotes then
				       Sql.print_orgs_as_notes stdout !prefix !orgfile formatted
				     else if !sql_update then
                                       Sql_update.print_orgs stdout !prefix !orgfile formatted !version_incr
				     else ()
				    )
				end;
			    end
			  else ()
			)
(* 				     else
				       let filtered =
					 if !extract = "" then formatted
					 else Orgfilter.filter_version !extract !prefix formatted
				       in
				       Org.print_orgs_raw stdout !prefix filtered;
				       LOG "Done!" LEVEL INFO
*)
		    | Version|Longhelp|Help -> () (* The ones have been match before. *)
		end
	end
    | None ->
      Arg.usage aligned usage_msg

let anon_fun = fun
  x ->
    if not (List.exists (fun (mode, arg, _) ->
      if x = mode then
	match arg with
	    Arg.Unit f -> f (); true
	  | _ -> false
      else 
	false
    ) modes) then
      freearg := x

let _ =
  Bolt.Logger.register "" Bolt.Level.INFO "all" "default" (Bolt.Mode.direct ())
    "file" ("<stderr>", {Bolt.Output.seconds_elapsed = None; Bolt.Output.signal_caught = None});
  LOG "*** START ***" LEVEL TRACE;
  Array.iteri (fun i opt -> LOG "Option %d: %s" i opt LEVEL TRACE) Sys.argv;
  let aligned = Arg.align options in
    (try
      Arg.parse_argv Sys.argv aligned anon_fun usage_msg;
    with Arg.Bad msg ->
      (LOG msg LEVEL FATAL; exit 0));
  (try main aligned
   with e ->
     LOG "Exception: %s" (Printexc.to_string e) LEVEL FATAL;
     Debug.trace (Printexc.get_backtrace ());
     exit 1);
  if !Debug.profile <> Debug.PNONE then
    Debug.trace (Debug.profile_diagnostic ());
  LOG "*** END ***" LEVEL TRACE

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
