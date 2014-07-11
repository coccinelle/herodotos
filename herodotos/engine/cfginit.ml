exception Misconfiguration of string


let get_project_name expProject=match expProject with Ast_config.ExpProject p->p

let get_pattern_name expPattern=match expPattern with Ast_config.ExpPattern p->p


let gen_cvsignore_prj v1 (p, _) =
  let prjdir = Config.get_prjdir p in
  let (_, varr) = Config.get_versinfos p in
  let cvsignore = !Setup.resultsdir ^ prjdir^"/.cvsignore" in
    if not (Sys.file_exists cvsignore) then
      if v1 then prerr_endline ("Generating "^ cvsignore);
      let cvsch = open_out cvsignore in
	Printf.fprintf cvsch "*%s\n" Global.correlext;
	Printf.fprintf cvsch "*%s\n" Global.listext;
	Printf.fprintf cvsch "Makefile\n";
	Printf.fprintf cvsch ".depend.%s\n" p;
	Array.iter (fun (vname, _, _, _) -> Printf.fprintf cvsch "%s\n" vname) varr;
	close_out cvsch

let gen_cvsignore v1 prjs =
  let cvsignore = !Setup.prefix ^"/.cvsignore" in
    List.iter (gen_cvsignore_prj v1) prjs;
    if not (Sys.file_exists cvsignore) then
      if v1 then prerr_endline ("Generating "^ cvsignore);
      let cvsch = open_out cvsignore in
	Printf.fprintf cvsch ".depend\n";
	Printf.fprintf cvsch ".depend.patterns\n";
	Printf.fprintf cvsch ".depend.erase\n";
	Printf.fprintf cvsch ".depend.graphs\n";
	Printf.fprintf cvsch ".jgrfiles\n";
	close_out cvsch

let gen_makefile_cpu depch (target, cmd) =
  Printf.fprintf depch "%s: #PER-CORE\n" target;
  Printf.fprintf depch "\t%s\n" cmd;
  target

let gen_makefile_vers verbose depch verscmds =
  List.fold_left (fun targets (target, (cmd, cpucmds)) ->
		    Misc.create_dir verbose (Filename.dirname target);
		    if cpucmds <> [] then
		      let cputargets = List.map (gen_makefile_cpu depch) cpucmds in
			Printf.fprintf depch "%s: %s #PER-PRJ-PATT-VERS\n" target (String.concat " " cputargets);
		    else
		      Printf.fprintf depch "%s: #PER-PRJ-PATT-VERS\n" target;
		    Printf.fprintf depch "\t%s\n" cmd;
		    target::targets
		 ) [] verscmds

let gen_makefile_patt verbose depch cmds =
  List.fold_left (fun datalist (data, verscmds) ->
		    if verbose then prerr_endline ("make will generate data in "^data);
		    let targets = gen_makefile_vers verbose depch verscmds in
		      Misc.create_dir verbose (Filename.dirname data);
		      Printf.fprintf depch "%s: %s #PER-PRJ-PATT\n" data (String.concat " " targets);
                      if targets <> [] then
			Printf.fprintf depch "\tcat $^ > $@\n\t-cat $(^:%%.orig.org=%%.log) > $(@:%%.orig.org=%%.log)\n"
		      else
			Printf.fprintf depch "\ttouch $@\n";
		      data::datalist
		 ) [] cmds

let gen_makefile v1 v2 prjs patts =
  let deps = List.map
    (fun (p, cmds) ->
       let prjdir = Config.get_prjdir p in
	 if prjdir = "" then prerr_endline ("*** WARNING *** project dir is not set for " ^ p);
	 let depend = !Setup.resultsdir ^ prjdir ^"/.depend."^p in
	 let depch = Misc.create_dir_and_open v1 depend in
	   if v1 then prerr_endline ("Generating "^ depend);
	   let datalist = gen_makefile_patt v2 depch cmds in
	     Printf.fprintf depch "%s: %s\n" p (String.concat " " datalist);
	     close_out depch;
	     depend
    ) prjs
  in
  let pattdepend = !Setup.prefix ^"/.depend.patterns" in
  let pattdepch = open_out pattdepend in
    if v1 then prerr_endline ("Generating "^ pattdepend);
    List.iter (fun (patt, data) ->
		 let datalist = String.concat " " data in
		   Printf.fprintf pattdepch "%s: %s\n" patt datalist;
	      ) patts;
    close_out pattdepch;
  let gphdepend = !Setup.prefix ^"/.depend.graphs" in
  let gphdepch = open_out gphdepend in
    if v1 then prerr_endline ("Generating "^ gphdepend);
    let gphlist = Setup.GphTbl.fold (fun name _ l -> name::l) Setup.graphs [] in
    let gphs = String.concat " " gphlist in
      Printf.fprintf gphdepch ".PHONY:: %s\n" gphs;
      Printf.fprintf gphdepch "%s: $(CONF)\n" gphs;
      Printf.fprintf gphdepch "\t$(HERODOTOS) $(FLAGS) -c $(CONF) $@\n";
      close_out gphdepch;
      let depend = !Setup.prefix ^"/.depend" in
      let depch = open_out depend in
	if v1 then prerr_endline ("Generating "^ depend);
	Printf.fprintf depch ".PHONY::";
	List.iter (fun (p,_) -> Printf.fprintf depch " %s" p) prjs;
	Printf.fprintf depch "\n\n";
	Printf.fprintf depch "projects: update ";
	List.iter (fun (p,_) -> Printf.fprintf depch " %s" p) prjs;
	Printf.fprintf depch "\n";
	List.iter (Printf.fprintf depch "-include %s\n") deps;
	Printf.fprintf depch "-include .depend.patterns\n";
	Printf.fprintf depch "-include .depend.erase\n";
	Printf.fprintf depch "-include .depend.graphs\n";
	close_out depch

(* Generation of .depend.erase *)
let erase_file_vers verbose verscmds =
  List.fold_left (fun targets (target, (_, cpucmds)) ->
		    if cpucmds <> [] then
		      let cputargets = List.map fst cpucmds in
			target::cputargets@targets
		    else
		      target::targets
		 ) [] verscmds

let erase_file_patt verbose cmds =
  List.fold_left (fun datalist (data, verscmds) ->
		    let targets = erase_file_vers verbose verscmds in
		      (data, targets)::datalist
		 ) [] cmds

let erase_file v1 v2 prjs patts =
  let depend = !Setup.prefix ^"/.depend.erase" in
  let depch = open_out depend in
    if v1 then prerr_endline ("Generating "^ depend);
    let targetlist =
    List.flatten (List.map
      (fun (p, cmds) ->
	 let datalist = erase_file_patt v2 cmds in
	 let (data, targets) = List.split datalist in
	 let flat_targets = List.flatten targets in
	 let data_cmds = List.map (fun x -> "\trm -f "^ x) data in
	 let target_cmds = List.map (fun x -> "\trm -f "^ x) flat_targets in
	   Printf.fprintf depch "erase-%s:\n%s" p (String.concat "\n" data_cmds);
	   Printf.fprintf depch "\n%s\n" (String.concat "\n" target_cmds);
	   datalist
      ) prjs)
  in
  List.iter (fun (patt, data) ->
	       let datalist = List.map (fun x -> "\trm -f "^ x) data in
		 Printf.fprintf depch "erase-%s:\n%s\n" patt (String.concat "\n" datalist);
		 List.iter (fun d ->
			      let targets = List.assoc d targetlist in
			      let cmds = List.map (fun x -> "\trm -f "^ x) targets in
				Printf.fprintf depch "%s\n" (String.concat "\n" cmds)
			   ) data;
	    ) patts;
    close_out depch

(* Check if a project is correctly configured *)
let check_prj vb p =
  let prjdir = Config.get_prjdir p in
  if prjdir = "" then prerr_endline ("*** WARNING *** project dir is not set for " ^ p);
  let project = !Setup.projectsdir ^ prjdir in
  let (depth, versions) = Config.get_versions p in
  List.fold_left (fun l v ->
    let prj = match Config.get_subdir p with
	None     -> project ^ "/" ^ v
      | Some dir -> project ^ "/" ^ v ^ "/" ^ dir
    in
    if not (Sys.file_exists prj) then
      ("Unable to access " ^ prj)::l
    else
      (
	if vb then
	  prerr_endline (prj ^ " - OK");
	l
      )
  ) [] versions

(* Check if a pattern is correctly configured *)
let check_pat vb d =
  let coccifile = Config.get_coccifile d in
  let cocci = !Setup.smatchdir ^"/" ^ coccifile in
  if not (Sys.file_exists cocci) then
    [("Unable to access " ^ cocci)]
  else
    (
      if vb then
	prerr_endline (cocci ^ " - OK");
      []
    )

(* Check whether the setup is ok according to the configuration file  *)
let check_setup vb prjs pats =
  let err =
    List.map (fun (p, _) -> check_prj vb p) prjs @
      List.map (fun (p, _) -> check_pat vb p) pats
  in
  List.flatten err

(* Prune redondant vers/prj/patt org target *)
let compute_make_for_target (targetlist: (string * (string * (string * string) list)) list) target =
  (target, List.assoc target targetlist)

(* Prune redondant prj/patt org target *)
let compute_make_for_data (dtlist: (string * (string * (string * (string * string) list)) list) list) data =
  let targets = List.find_all (fun (d, _) -> d = data) dtlist in
  let targettuplelist = snd (List.split targets) in
  let targetmerge = List.flatten targettuplelist in
  let targetlist = fst (List.split targetmerge) in
  let uniquelist = Misc.unique_list targetlist in
  let targetset = List.map (compute_make_for_target targetmerge) uniquelist in
    (data, targetset)

(* Prune redondant patt org target *)
let compute_make_for_prj (cmds: ((string * string) * (string * (string * (string * (string * string) list)) list)) list) prj =
  let prjcmds = List.find_all (fun ((prjdir,_), _) -> prj = prjdir) cmds in
  let datatuplelist = snd (List.split prjcmds) in
  let datalist = fst (List.split datatuplelist) in
  let uniquelist = Misc.unique_list datalist in
  let data = List.map (compute_make_for_data datatuplelist) uniquelist in
    (prj, data)

(* Prune redondant prj org target *)
let compute_make_for_patt (cmds: ((string * string) * (string * (string * (string * (string * string) list)) list)) list) patt =
  let pattcmds = List.find_all (fun ((_, pattname), _) -> patt = pattname) cmds in
  let datatuplelist = snd (List.split pattcmds) in
  let datalist = fst (List.split datatuplelist) in
  let data = Misc.unique_list datalist in
    (patt, data)

(* Prune redondant prj org target *)
let comp_makefile_for_curves atts curves =
  List.flatten (List.map (Config.get_cmdlist atts) curves)

let comp_makefile_for_group atts group =
  match group with
      Ast_config.GrpCurve (_, curves) ->
	comp_makefile_for_curves atts curves
    | Ast_config.GrpPatt (patt, _) ->
	let projects = Config.get_projects atts in
	let dummy_curves =
	  List.map (fun prj -> (Some prj, Some patt,[],Misc.dummy_pos)) projects
	in comp_makefile_for_curves atts dummy_curves


(*equivalent for experiences *)
let comp_makefile_for_exp (experience:Ast_config.experience) =
  let (se1,se2) = experience in
  match se1 with
      Ast_config.ObjPatt patts ->
	let lpatts=List.map(get_pattern_name) patts in
        begin
          match se2 with 
              Ast_config.ObjProj projs ->
		let lprojs=List.map(get_project_name) projs in
                List.map(fun p->
                  Config.get_cmdList p lpatts) lprojs
            |_ -> raise (Misconfiguration "A pattern list must be given")
        end
    | Ast_config.ObjProj projs ->
      let lprojs = List.map(get_project_name) projs in
      match se2 with
          Ast_config.ObjPatt patts ->
	    let lpatts=List.map(get_pattern_name) patts in
            List.map(fun p->
              Config.get_cmdList p lpatts) lprojs 
        |_ -> raise (Misconfiguration "A pattern list must be given")

let comp_makefile_for_experiences experience =
  List.flatten (comp_makefile_for_exp experience)

let compute_makefile () =
  let cmdslist =  Setup.GphTbl.fold
    (fun name (atts, subgraph) cmds ->
       match Config.get_ytype name atts with
	   "size" | "sizepct" | "usersize" -> cmds
	   | _ ->
	       begin
		 match subgraph with
		     Ast_config.Curves curves ->
		       comp_makefile_for_curves atts curves :: cmds
		   | Ast_config.Groups groups ->
		       List.map (comp_makefile_for_group atts) groups @ cmds
	       end
    ) Setup.graphs []
  in
  let cmdslist_exp = Setup.ExpTbl.fold
    (fun name_exp exp cmds-> (comp_makefile_for_experiences (Setup.ExpTbl.find Setup.experiences name_exp))::cmds)
    Setup.experiences [] in
  let cmdslist=cmdslist@cmdslist_exp in
  let cmds = List.flatten cmdslist in
  let prjpatt = fst (List.split (cmds)) in
  let (fullprjs, fullpatts) = List.split prjpatt in
  let prjs = Misc.unique_list fullprjs in
  let patts = Misc.unique_list fullpatts in
    (
      List.map (compute_make_for_prj cmds) prjs,
      List.map (compute_make_for_patt cmds) patts
    )

let init_env v1 v2 v3 configfile cvs =
  ignore(Config.parse_config configfile);
  LOG "Config parsing OK!" LEVEL INFO;
  Config.show_config ();
  let (prjs, patts) = compute_makefile () in
  let err = check_setup v1 prjs patts in
  if err = [] then
    (
      gen_makefile v1 v2 prjs patts;
      erase_file v1 v2 prjs patts;
      if cvs then gen_cvsignore v1 prjs
    )
  else
    (
      prerr_endline " *** KNOWN PROBLEMS ***";
	List.iter (fun msg -> prerr_endline msg) err;
      prerr_endline " *** END OF REPORT ***"
    )
