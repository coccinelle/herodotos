exception Unimplemented of string
exception Failed

let svgfiles = ref []

let make_eps jgrname epsname =
    prerr_string ("Generation of "^epsname);
    let status = Unix.system ("jgraph "^jgrname^" | ps2eps -q -l -r 600 -g -C > "^epsname) in
      match status with
	  Unix.WEXITED 0 -> prerr_endline (" - OK")
	| _         -> prerr_endline (" - KO"); raise Failed

let make_pdf jgrname pdfname =
    prerr_string ("Generation of "^pdfname);
    let status = Unix.system ("jgraph "^jgrname^" | ps2eps -q -l -r 600 -g -C | epstopdf --filter > "^pdfname) in
      match status with
	  Unix.WEXITED 0 -> prerr_endline (" - OK")
	| _         -> prerr_endline (" - KO"); raise Failed

let make_png quality pdfname pngname =
    prerr_string ("Generation of "^pngname);
    let status = Unix.system ("convert -density "^ (string_of_int quality) ^" "^pdfname^" "^pngname) in
      match status with
	  Unix.WEXITED 0 -> prerr_endline (" - OK")
	| _         -> prerr_endline (" - KO"); raise Failed

let make_svg () =
  let isch = Unix.open_process_out "inkscape --shell -z 2> /dev/null > /dev/null" in
  List.iter (fun (pdf ,svg) ->
	       prerr_endline ("Generation of "^svg);
	       Printf.fprintf isch "%s -l %s\n" pdf svg;
	       flush isch
	    ) !svgfiles;
    Printf.fprintf isch "quit\n";flush isch;
    let status = Unix.close_process_out isch in
      match status with
	  Unix.WEXITED 0 ->
	    List.iter (fun (_ ,svg) -> Html.register_svg svg) !svgfiles;
	    prerr_endline ("Generation of all SVG - OK")
	| _         -> prerr_endline ("Generation of all SVG - KO")

let gen_graph v1 v2 conn eps pdf png web name graph =
  let (atts, subgraph) = graph in
    if v1 then print_endline ("Graph "^name);
    let jgrname =
      match subgraph with
	  Ast_config.Curves curves ->
	    Helper.draw v1 v2 conn name User.dfts (atts, curves)
	| Ast_config.Groups groups ->
	    Group.draw v1 v2 conn name User.dfts (atts, groups)
    in
      if pdf then
	let pdfname = Str.replace_first (Str.regexp_string ".jgr")".pdf" jgrname in
	  try
	    make_pdf jgrname pdfname;
	    if web || png then
	      begin
		let pngname = Str.replace_first (Str.regexp_string ".jgr")".png" jgrname in
		let pngpath = !Setup.websitedir ^"/" ^(Filename.basename pngname) in
		let smallpngpath = !Setup.websitedir ^"/small_" ^(Filename.basename pngname) in
		let htmlrootpath = Str.replace_first (Str.regexp_string ".png")"" pngpath in
		let svgpath = Str.replace_first (Str.regexp_string ".png")".svg" pngpath in
		  (try
                     Misc.create_dir v1 !Setup.websitedir;
		     make_png 600 pdfname pngpath;
		     make_png 100 pdfname smallpngpath;
		     Html.register htmlrootpath;
		     Html.register_png pngpath
		   with Failed -> ()
		  );
		  svgfiles := (pdfname,svgpath) :: !svgfiles
	      end
	  with Failed -> ()
      else
	if eps then
	  let epsname = Str.replace_first (Str.regexp_string ".jgr")".eps" jgrname in
	    try
	      make_eps jgrname epsname;
	    with Failed -> ()
	else prerr_string ("Generation of "^jgrname^" - OK")
	  
let gen_graph_nofail v1 v2 conn eps pdf png web name graph =
  Debug.profile_code "Engine.gen_graph_nofail"
    (fun () ->
       try
	 try
	   try
	     gen_graph v1 v2 conn eps pdf png web name graph
	   with Unimplemented msg
	     | Defects.Unsupported msg
	     | Config.Misconfiguration msg ->
		 prerr_endline msg;
		 prerr_endline "Skiping graph..."
	 with Config.Misconfigurationat (msg, pos) ->
	   Misc.print_error pos ("*** ERROR *** "^ msg);
	   prerr_endline "Skiping graph..."
       with e ->
	 prerr_endline ("*** ERROR *** While processing "^ name);
	 Printexc.print_backtrace stderr;
	 prerr_endline "Skiping graph..."
    )

let gen_graphs v1 v2 v3 config conn eps pdf png web conn freearg =
  if web || png then Misc.create_dir v2 !Setup.websitedir;
  (
    if freearg = "" then
      Setup.GphTbl.iter (gen_graph_nofail v1 v2 conn eps pdf png web) Setup.graphs
    else
      try
	gen_graph_nofail v1 v2 conn eps pdf png web freearg (Setup.GphTbl.find Setup.graphs freearg)
      with Not_found ->
	prerr_endline ("No graph named "^ freearg^" found!\nCheck "^config^" to fix the error.")
  );
  if web then
    (
      make_svg ();
      (* FIXME Properly generate HTML website even if freearg is set *)
      if freearg = "" then Html.gen_site !Setup.websitedir config
      else prerr_endline ("*** WARNING *** Refreshed of a single graph. NOT refreshing the HTML pages!")
    )

let run v1 v2 v3 config eps pdf png web freearg =
  begin
    ignore(Config.parse_config config);
    if v1 then prerr_endline "Config parsing OK!";
    Config.fixcolor ();
    if v1 then Config.show_config v2 v3;

    if v1 then prerr_endline ("Connecting to "^ !Setup.dbconn);
    try
      let conn = Database.open_db v1 !Setup.dbconn in
	if v1 then prerr_endline "Connection - OK !";
	gen_graphs v1 v2 v3 config conn eps pdf png web conn freearg;
	if v1 then prerr_endline "Disconnecting...";
	Database.close_db conn;
	if v1 then prerr_endline "Done."
    with Postgresql.Error (e) ->
      if v1 then prerr_endline "Connection - KO !";
      match e with
	| Postgresql.Field_out_of_range (_, _)
	| Postgresql.Tuple_out_of_range (_, _)
	| Postgresql.Binary
	  ->
	    prerr_endline "Unexpected failure"
	| Postgresql.Connection_failure (s)
	| Postgresql.Unexpected_status ( _, s, _)
	| Postgresql.Cancel_failure (s)
	  ->
	    prerr_endline s
  end
