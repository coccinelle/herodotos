exception Unimplemented of string
exception Failed

let svgfiles = ref []

let ytypes_register =
  let tbl = Hashtbl.create 31 in
    List.iter (fun (key, dfts) -> Hashtbl.add tbl key dfts)
   [("avgage"     , Avgage.dfts       );
    ("avglifespan", Avglifespan.dfts  );
    ("birth"      , Birth.dfts	      );
    ("birthfile"  , Birthfile.dfts    );
    ("cumulcount" , Count.dfts        );
    ("death"      , Death.dfts	      );
    ("deathfile"  , Deathfile.dfts    );
    ("density"    , Ratioevol.dfts    );
    ("eldest"     , Eldest.dfts	      );
    ("eldestdeath", Eldestdeath.dfts  );
    ("expertise"  , Expertise.dfts    );
    ("patchexpert", Patchexpert.dfts  );
    ("timeexpert" , Timeexpert.dfts   );
    ("abstime"    , Abstime.dfts      );
    ("reltime"    , Reltime.dfts      );
    ("lifeexpect" , Lifeexpect.dfts   );
    ("netincrease", Netincrease.dfts  );
    ("size"       , Size.dfts	      );
    ("sizepct"    , Sizepct.dfts      );
    ("count"      , Countevol.dfts    );
    ("user"       , User.dfts         );
    ("usersize"   , Size.dfts         )];
    tbl

let keys hashtbl =
  Hashtbl.fold (fun key _ acc -> key::acc) hashtbl []

let supported_types =
  "Supported types are: "^
    String.concat " " (List.sort compare (keys ytypes_register))

let get_dft graphtype =
  try
    Hashtbl.find ytypes_register graphtype
  with Not_found ->
    raise (Unimplemented (
	     "Graphs of type \"" ^ graphtype^
	       "\" are currently unknown/unimplemented.\n"^
	       supported_types
	   ))

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
(*
let make_svg pdfname svgname =
    prerr_string ("Generation of "^svgname);
    let status = Unix.system ("inkscape "^pdfname^" -l "^svgname^" 2> /dev/null") in
      match status with
	  Unix.WEXITED 0 -> prerr_endline (" - OK")
	| _         -> prerr_endline (" - KO"); raise Failed
*)

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

let gen_graph v1 v2 pdf png web bugs name graph =
  let (atts, subgraph) = graph in
    if v1 then print_endline ("Graph "^name);
    let jgrname =
      match subgraph with
	  Ast_config.Curves curves ->
	    let gtype = Graph.get_ytype name atts in
	      if gtype = "occurrences" then
		Defects.draw v1 v2 name (atts, curves) bugs
	      else
		Helper.draw v1 v2 name (get_dft gtype) (atts, curves) bugs
	| Ast_config.Groups groups ->
	    let gtype = Graph.get_ytype name atts in
	      Group.draw v1 v2 name (get_dft gtype) (atts, groups) bugs
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
	let epsname = Str.replace_first (Str.regexp_string ".jgr")".eps" jgrname in
	  try
	    make_eps jgrname epsname;
	  with Failed -> ()

let gen_graph_nofail v1 v2 pdf png web bugs name graph =
  Debug.profile_code "Cfgmode.gen_graph_nofail"
    (fun () ->
       try
	 try
	   try
	     gen_graph v1 v2 pdf png web bugs name graph
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

let graph_gen v1 v2 v3 configfile pdf png web freearg =
  begin
    ignore(Config.parse_config configfile);
    LOG "Config parsing OK!" LEVEL INFO;
    Config.fixcolor ();
    Config.show_config ();
    let bugs = Cfghelper.compute_graphs v2 freearg in
      Cfghelper.genMakefile ();
      if web || png then Misc.create_dir v2 !Setup.websitedir;
      (
	if freearg = "" then
	  Setup.GphTbl.iter (gen_graph_nofail v1 v2 pdf png web bugs) Setup.graphs
	else
	  try
	    gen_graph_nofail v1 v2 pdf png web bugs freearg (Setup.GphTbl.find Setup.graphs freearg)
	  with Not_found ->
	    prerr_endline ("No graph named "^ freearg^" found!\nCheck "^configfile^" to fix the error.")
      );
      if web then
	(
	  make_svg ();
	  (* FIXME Properly generate HTML website even if freearg is set *)
	  if freearg = "" then Html.gen_site !Setup.websitedir configfile
	  else prerr_endline ("*** WARNING *** Refreshed of a single graph. NOT refreshing the HTML pages!")
	)

  end
