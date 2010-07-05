
exception Misconfigured

let configfile = ref ""
let help = ref false
let longhelp = ref false
let version = ref false
let verbose1 = ref false
let verbose2 = ref false
let verbose3 = ref false
let graphs = ref false
let eps = ref true
let png = ref false
let pdf = ref true
let web = ref false

let freearg = ref ""

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " -c <file>\n"

let options = [
  "-c", Arg.Set_string configfile, "file Configuration file";
  "--config", Arg.Set_string configfile, "file Configuration file";
  "--debug", Arg.Set Misc.debug, " Debug mode";
  "--graphs", Arg.Set graphs, " List graphs";
  "-h", Arg.Set help, " Display this list of options";
  "-help", Arg.Set help, " Display this list of options";
  "--help", Arg.Set help, " Display this list of options";
  "--longhelp", Arg.Set longhelp, " Display this list of options and the supported graph types";
  "--eps", Arg.Clear pdf, " disable the (default) generation of PDF with 'epstopdf'";
  "--jgr", Arg.Unit (fun _ ->  pdf:= false; eps:=false), " disable the (default) generation of EPS and PDF";
  "--png", Arg.Set png, " enable the generation of png images (in default mode)";
  "--profile", Arg.Unit (fun () -> prerr_endline "*** PROFILING ENABLED ***";
			   Debug.profile := Debug.PALL), " gather timing information about the main functions";
  "--version", Arg.Set version, " Print Herodotos version";
  "-v", Arg.Set verbose1, " verbose mode";
  "-vv", Arg.Set verbose2, " more verbose mode";
  "-vvv", Arg.Set verbose3, " more more verbose mode";
  "-w", Arg.Set web, " generation of the website (in default mode)";
  "--web", Arg.Set web, " generation of the website (in default mode)"
]


let main aligned =
  if !version then
    print_endline ("sql2jgr version "^ Global.version)
  else
    if !configfile <> "" then
      begin
	if !verbose3 then verbose2 := true;
	if !verbose2 then verbose1 := true;
	if (!help || !longhelp) then
	  (
	    Arg.usage aligned usage_msg;

	    if !longhelp then
	      prerr_endline ("\nTODO: PLACE LONGHELP HERE\n")
	  )
	else
	  if !graphs then
	    (
	      ignore(Config.parse_config !configfile);
	      Setup.GphTbl.iter (fun name _ -> print_endline name) Setup.graphs
	    )
	  else
	    (
	      print_endline ("sql2jgr version "^ Global.version);
	      prerr_endline ("Processing "^ !configfile);
	      Engine.run !verbose1 !verbose2 !verbose3 !configfile !eps !pdf !png !web !freearg;
	      prerr_newline ()
	    )
      end
    else
      Arg.usage aligned usage_msg

let _ =
  let aligned = Arg.align options in
    (try
       Arg.parse_argv Sys.argv aligned (fun x -> freearg := x) usage_msg;
     with Arg.Bad msg ->
       (prerr_string msg; exit 0));
    main aligned;
    prerr_endline (Debug.profile_diagnostic ())
