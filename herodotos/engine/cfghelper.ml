exception Error of string

let get_bugset_of_group atts group : string list =
    match group with
	Ast_config.GrpCurve (_, curves) ->
	  List.flatten (List.map (Config.get_bugset_of_curve atts) curves)
      | Ast_config.GrpPatt (patt, pos) ->
	  let projects = Config.get_projects atts in
	    List.map (Config.get_bugset_of_grp_patt patt pos) projects

let get_bugset_of_graph name (atts, subgraph) =
  match subgraph with
      Ast_config.Curves curves ->
	(match Config.get_ytype name atts with
	     (** There is no bug set to look for when we draw code size **)
	     "size" | "sizepct" | "usersize" -> []
	   | _      ->
	       let orgs = List.map (Config.get_bugset_of_curve atts) curves in
		 (List.flatten orgs)
	)
    | Ast_config.Groups groups ->
	let orgs = List.map (get_bugset_of_group atts) groups in
	  (List.flatten orgs)

let get_all_bugset () =
  let (graphs, bugsets) =
    List.split
      (Setup.GphTbl.fold
	 (fun name datagraph orgset ->
	    (name, get_bugset_of_graph name datagraph)::orgset
       ) Setup.graphs []
    )
  in
    (graphs, List.flatten bugsets)

let check_filter filter file =
(* Check whether the filter is for a particular prj/patt pair *)
  filter = file
  || (let origfile = Filename.chop_suffix filter Global.origext in
      let new_filter = origfile ^ Global.bugext in
	new_filter = file
     )
  || (let new_filter = filter ^ Global.bugext in
	new_filter = file
     )
(* It could also be a "generic" filter about a prj or a patt *)
  ||
    (Str.string_match (Str.regexp_string filter) file 0)

let get_bugset filter : string list =
  let orgs =
    if filter = "" then
      snd (get_all_bugset ())
    else
      try
	let datagraph = Setup.GphTbl.find Setup.graphs filter in
	  get_bugset_of_graph filter datagraph
      with Not_found ->
	let (graphs, fullbugset) = get_all_bugset () in
	let bugset = List.filter (check_filter filter) fullbugset in
	  if bugset <> [] then
	    bugset
	  else
	    begin
	      prerr_endline ("*** ERROR *** No graph or file named "^filter);
	      prerr_endline (" - GRAPH LIST: (Use --debug to have file list)");
	      List.iter (fun g -> prerr_endline ("\t"^g)) graphs;
	      if !Misc.debug then
		begin
		  prerr_endline (" - BUG SET LIST");
		  List.iter (fun f -> prerr_endline ("\t"^f)) fullbugset;
		end;
	      exit 2
	    end
  in
    Misc.unique_list orgs

let by_project p bug =
  let (pb, _) = bug in
    p = pb

let retrieve_exists verbose prjname mode bugsetfile vlist filelist =
  let prjdir = Config.get_prjdir prjname in
  let existfile = Str.replace_first (Str.regexp_string Global.bugext) Global.existext bugsetfile in
  let project = !Setup.projectsdir ^ prjdir in
  let bugstat =
    if Sys.file_exists bugsetfile
    then (Unix.stat bugsetfile).Unix.st_ctime
    else
      let bugfile  = Filename.chop_suffix bugsetfile Global.bugext in
      let origfile = bugfile ^ Global.origext in
	(Unix.stat origfile).Unix.st_ctime
  in
  let existstat =
    if Sys.file_exists existfile
    then (Unix.stat existfile).Unix.st_ctime
    else bugstat
  in
  let fromfile =
    if bugstat < existstat
      then
	try
	  let ret = Some (Exists.parse_exist existfile) in
	    if verbose then prerr_endline ("Cache file "^ existfile^" was used.");
	    ret
	with
	    Sys_error msg -> prerr_endline msg; None
	  | Exists.BadFormat -> None
    else
      (if verbose then prerr_endline ("Cache file "^ existfile^" is obsolete or does not exist.");
       None)
  in
    match fromfile with
	Some ast -> ast
      | None ->
	  prerr_endline ("*** INFO *** Compute "^existfile^" from repository "^ !Setup.projectsdir ^ prjdir ^".");
	  let ast = Exists.analyze_rep project vlist filelist in
	    Exists.print_to existfile ast;
	    ast

let process_org  verbose prjname mode bsf vlist varray bfl =
  let filelist = Bugs.get_buggyfiles bfl in
  let ast_exist = retrieve_exists verbose prjname mode bsf vlist filelist in
    try
      let (max_ver, fileexist) = Exists.compute_exist varray ast_exist in
	(bsf, (varray, fileexist, bfl, prjname))
    with Exists.Misconfiguration msg ->
      raise (Error ("*** ERROR *** Processing "^prjname ^": "^bsf^"\n"^ msg))

let process_csv verbose prjname bsf varray =
  let entries = Csv.parse_csv bsf in
  let counts = List.map
    (fun (version, count_list) ->
       match count_list with
	   [count] -> (version, Csv.int_of_state count, 0)
	 | [c1;c2] -> (version, Csv.int_of_state c1, Csv.int_of_state c2)
	 |_ ->
	    raise (Error ("*** ERROR *** Processing "^prjname ^": "^bsf^"\n"^
			 "Does your file have multiple information?\n"^
			 "Expected format is \"version name;occurrence count;\""))
    ) entries
  in
    (bsf, (varray, counts, [], prjname))

let compute_pbugs verbose prjname allbugs =
  try
    let (_, vlist) = Config.get_versions prjname in
    let (_, varray) = Config.get_versinfos prjname in
    let pbugs = List.filter (by_project prjname) allbugs in
    let bugs = (snd(List.split pbugs)) in
      Some (List.map
	      (fun ((format, mode,bsf), bfl) ->
		 match format with
		     Ast_config.Org ->
		       process_org  verbose prjname mode bsf vlist varray bfl
		   | Ast_config.CSV ->
		       process_csv verbose prjname bsf varray
	      ) bugs
	   )
  with Config.Warning msg
    | Error msg ->
	prerr_endline msg;
	None

let compute_bugs verbose bugfile =
  try
    (*		  let (pdir, _) = Misc.strip_prefix (!Setup.resultsdir^"/") bugfile in
		  let p = Config.rev_prjdir pdir in
    *)
    let file = Filename.chop_suffix (Filename.basename bugfile) Global.bugext in
    let (p,patt) =
      (* TODO: Fixme. This may not work according to the names of the projects *)
      match Str.split (Str.regexp_string Global.sep) file with
	  [] -> raise (Error ("Malformed bug filename: "^bugfile))
	| p::tail -> (p,String.concat Global.sep tail)
    in
    let pdir = Config.get_prjdir p in
    let (depth, vlist) = Config.get_versinfos p in
    let prefix = !Setup.projectsdir ^ pdir ^"/" in
    let mode = Config.get_correl_mode verbose patt in
    let format = Config.get_format patt in
    let bugs =
      if format = Ast_config.CSV then
	[("", "", [], (0,0,0))]
      else
	if Sys.file_exists bugfile && format = Ast_config.Org
	then
	  (if verbose then prerr_endline ("Analyzing "^bugfile  ^ " in project " ^ p ^ " with subproject depth "^string_of_int depth ^ ".");
	   let orgbugs = Org.parse_org false bugfile in
	   let bugs = Org2bug.convert prefix depth orgbugs in
	     bugs
	  )
	else
	  begin
	    if (mode = Ast_config.Nocorrel && format = Ast_config.Org) then
	      begin
		let bugfile = Filename.chop_suffix bugfile Global.bugext in
		let orgfile = bugfile ^ Global.origext in

		let orgs  = Org.format_orgs prefix depth (Org.parse_org false orgfile) in
		  List.map (Correl.fast_bug prefix vlist) orgs
	      end
	    else
	      (prerr_endline ("*** ERROR *** No file " ^bugfile);
	       []
	      )
	  end
    in
      if bugs = []
      then None
      else
	let (count, unsortedbfl) = List.fold_left
	  (fun (count, acc) b ->
	     let report = Bugs.compute_bug vlist b in
	     let (_, _, vmin, vmax, _) = report in
	       if vmin = -1 || vmax = -1 then
		 (count+1, acc)
	       else
		 (count, report::acc)
	  ) (0, []) bugs
	in
	  if count <> 0 then
	    prerr_endline ("*** WARNING *** Dropping "^string_of_int count^" bug report(s) of "^bugfile^" (No interesting version) !");
	  let bfl = Bugs.sort unsortedbfl in
	    Some (p, ((format, mode, bugfile), bfl))

(*
with
	    Sys_error msg ->
	      prerr_endline ("*** ERROR *** Failure with " ^bugfile ^": "^msg);
	      None
	  | Failure msg ->
	      prerr_endline ("*** ERROR *** Failure with " ^bugfile ^": "^msg);
	      None
	)
*)
  with Config.Warning msg ->
    prerr_endline ("*** WARNING *** " ^ msg);
    None

let compute_graphs verbose graph =
  Debug.profile_code "Cfghelper.compute_graphs"
    (fun () ->
       let bugfiles = get_bugset graph in
	 if verbose then
	   begin
	     prerr_endline "==========================";
	     List.iter (fun d -> prerr_endline d) bugfiles;
	     prerr_endline "=========================="
	   end;
	 let pbugs =
	   List.fold_left (fun pbugs bugfile ->
			     match compute_bugs verbose bugfile with
				 Some v -> v::pbugs
			       | None -> pbugs
			  ) [] bugfiles
	 in
	   List.flatten (
	     Setup.PrjTbl.fold
	       (fun name _ set ->
		  match compute_pbugs verbose name pbugs with
		      Some v -> v::set
		    | None -> set
	       ) Setup.projects []
	   )
    )

let genMakefile () =
  Debug.profile_code "Cfghelper.genMakefile"
    (fun () ->
       let keys = Setup.GphTbl.fold (fun name _ keys -> name::keys) Setup.graphs [] in
       let jobs = String.concat " " keys in
       let makefile = !Setup.prefix ^ "/.jgrfiles" in
       let ch = open_out makefile in
	 output_string ch ("IMAGES="^jobs^"\n");
	 close_out ch
    )
