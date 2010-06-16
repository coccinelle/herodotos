
exception Error of string

let help = ref false
let orgfile = ref ""
let prefix = ref ""
let version = ref false
let vb = ref false

let notes = ref false

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [ --notes ] --prefix <prefix> <Org file>\n"

let options = [
  "--debug", Arg.Set Misc.debug, " Debug mode";
  "-h", Arg.Set help, " Display this list of options";
  "-help", Arg.Set help, " Display this list of options";
  "--help", Arg.Set help, " Display this list of options";
  "--prefix", Arg.Set_string prefix, "path prefix of the source directories (test)";
  "--profile", Arg.Unit (fun () -> prerr_endline "*** PROFLING ENABLED ***";
			   Debug.profile := Debug.PALL), " gather timing information about the main functions";
  "--version", Arg.Set version, " Print Herodotos version";
  "-v", Arg.Set vb, " verbose mode";
]

let convert vb orgfile prefix =
  Debug.profile_code_silent "convert"
    (fun () ->
       try
	 let in_ch = if orgfile <> "-" then open_in orgfile else stdin in
	 let _exit = ref false in
	 let line = ref 0 in
	   while not !_exit do
	     try
	       match Org.parse_org vb orgfile line in_ch with
		   None -> ()
		 | Some org ->
		     if !notes then
		       Sql.print_org_as_note stdout prefix orgfile org
		     else
		       Sql.print_org stdout prefix orgfile org
	     with
		 End_of_file -> _exit := true
	   done;
	   close_in in_ch;
       with Sys_error msg ->
	 prerr_endline ("*** WARNING *** "^msg)
    )

let main aligned =
  if !version then
    print_endline ("Org2SQL version "^ Global.version)
  else
    begin
      if !help then
	Arg.usage aligned usage_msg
      else
	(* For Org file parsing *)
	(* For converting Org file to SQL entries *)
	if ((String.length !orgfile) <> 0)
	  && ((String.length !prefix) <> 0)
	then
	  convert !vb !orgfile !prefix
	else
	  Arg.usage aligned usage_msg
    end

let _ =
  let aligned = Arg.align options in
    (try
      Arg.parse_argv Sys.argv aligned (fun x -> orgfile := x) usage_msg;
    with Arg.Bad msg ->
      (prerr_string msg; exit 0));
    main aligned;
    if !Debug.profile = Debug.PALL then prerr_endline (Debug.profile_diagnostic ())
