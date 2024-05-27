exception Misconfiguration of string


let get_project_name expProject=match expProject with Ast_config.ExpProject p->p

let get_pattern_name expPattern=match expPattern with Ast_config.ExpPattern p->p


let gen_cvsignore_prj v1 (p, _) =
  let prjdir = Config.get_prjdir p in
  let (_, varr) = Config.get_versinfos p in
  let cvsignore = !Setup.resultsdir ^ prjdir^"/.cvsignore" in
  if not (Sys.file_exists cvsignore) then
    let logmsg=Printf.sprintf "Generating %s" cvsignore in
      [%info_log logmsg];
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
      let logmsg=Printf.sprintf "Generating %s" cvsignore in
      [%info_log logmsg];
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
      let logmsg=Printf.sprintf "make will generate data in %s" data in
    [%info_log logmsg];
    let targets = gen_makefile_vers verbose depch verscmds in
    Misc.create_dir verbose (Filename.dirname data);
    Printf.fprintf depch "%s: %s #PER-PRJ-PATT\n" data (String.concat " " targets);
    Printf.fprintf depch "\ttouch $@\n";
    data::datalist
  ) [] cmds

let gen_makefile v1 v2 prjs patts exps =
  let deps = List.map
    (fun (p, cmds) ->
       let prjdir = Config.get_prjdir p in
       if prjdir = "" then (let logmsg=Printf.sprintf "*** WARNING *** project dir is not set for %s" p in [%warn_log logmsg]);
       let depend = !Setup.resultsdir ^ prjdir ^"/.depend."^p in
       let depch = Misc.create_dir_and_open v1 depend in
       let logmsg2=Printf.sprintf "Generating %s" depend in
       [%info_log logmsg2];
       let datalist = gen_makefile_patt v2 depch cmds in
       Printf.fprintf depch "%s: %s\n" p (String.concat " " datalist);
       close_out depch;
       depend
    ) prjs
  in
  let pattdepend = !Setup.prefix ^"/.depend.patterns" in
  let pattdepch = open_out pattdepend in
  let logmsg3=Printf.sprintf "Generating %s" pattdepend in
  [%info_log logmsg3];
  List.iter (fun (patt, data) ->
    let datalist = String.concat " " data in
    Printf.fprintf pattdepch "%s: %s\n" patt datalist;
  ) patts;
  close_out pattdepch;
  let gphlist = Setup.GphTbl.fold (fun name _ l -> name::l) Setup.graphs [] in
  let gphs = String.concat " " gphlist in
  let expnames = String.concat " " (fst (List.split exps)) in
  let depend = !Setup.prefix ^"/.depend" in
  let depch = open_out depend in
  let logmsg4=Printf.sprintf "Generating %s" depend in
  [%info_log logmsg4];
  Printf.fprintf depch ".PHONY:: %s %s" gphs expnames;
  List.iter (fun (p,_) -> Printf.fprintf depch " %s" p) prjs;
  Printf.fprintf depch "\n\n";
  if gphlist <> [] then (
    Printf.fprintf depch "%s: $(CONF)\n" gphs;
    Printf.fprintf depch "\t$(HERODOTOS) $(FLAGS) -c $(CONF) graph $@\n\n"
  );
  Printf.fprintf depch "projects: update ";
  List.iter (fun (p,_) -> Printf.fprintf depch " %s" p) prjs;
  Printf.fprintf depch "\n";
  List.iter (Printf.fprintf depch "-include %s\n") deps;
  Printf.fprintf depch "-include .depend.patterns\n";
  Printf.fprintf depch "-include .depend.erase\n\n";
  List.iter (fun (name, cmds) ->
    let deps = String.concat " " cmds in
    Printf.fprintf depch "%s: $(CONF) %s\n\n" name deps
  ) exps;
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
  let logmsg=Printf.sprintf "Generating %s" depend in
  [%info_log logmsg];
  let targetlist =
    List.flatten
      (List.map
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
  if prjdir = "" then (let logmsg=Printf.sprintf "*** WARNING *** project dir is not set for %s" p in [%warn_log logmsg]);
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
        let logmsg=Printf.sprintf "%s - OK" prj in
	[%trace_log logmsg];
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
      let logmsg=Printf.sprintf "%s - OK" cocci in
      [%trace_log logmsg];
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
let compute_make_for_prj (cmds: ((Setup.PrjTbl.key * Setup.DftTbl.key) * (string * (string * (string * (string * string) list)) list)) list) prj =
  let prjcmds = List.find_all (fun ((prjdir,_), _) -> prj = prjdir) cmds in
  let datatuplelist = snd (List.split prjcmds) in
  let datalist = fst (List.split datatuplelist) in
  let uniquelist = Misc.unique_list datalist in
  let data = List.map (compute_make_for_data datatuplelist) uniquelist in
    (prj, data)

(* Prune redondant prj org target *)
let compute_make_for_patt (cmds: ((Setup.PrjTbl.key * Setup.DftTbl.key)  * (string * (string * (string * (string * string) list)) list)) list) patt =
  let pattcmds = List.find_all (fun ((_, pattname), _) -> patt = pattname) cmds in
  let datatuplelist = snd (List.split pattcmds) in
  let datalist = fst (List.split datatuplelist) in
  let data = Misc.unique_list datalist in
    (patt, data)

(* Prune redondant org target*)
let compute_make_for_exp (exp: (Setup.ExpTbl.key * ((Setup.PrjTbl.key * Setup.DftTbl.key)  * (string * (string * (string * (string * string) list)) list)) list)) : string * string list =
  let (name, cmds) = exp in
  let datatuplelist = snd (List.split cmds) in
  let datalist = fst (List.split datatuplelist) in
  let data = Misc.unique_list datalist in
    (name, data)

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


(* Equivalent for experiments *)
let comp_makefile_for_exp (experiment:Ast_config.experiment) =
  let (se1, se2) = experiment in
  match se1 with
      Ast_config.ObjPatt patts ->
	let lpatts = List.map get_pattern_name patts in
        begin
          match se2 with 
              Ast_config.ObjProj projs ->
		let lprojs = List.map get_project_name projs in
                List.map (fun p -> Config.get_cmdList p lpatts) lprojs
            |_ -> raise (Misconfiguration "A pattern list must be given")
        end
    | Ast_config.ObjProj projs ->
      let lprojs = List.map get_project_name projs in
      match se2 with
          Ast_config.ObjPatt patts ->
	    let lpatts = List.map get_pattern_name patts in
            List.map (fun p -> Config.get_cmdList p lpatts) lprojs 
        |_ -> raise (Misconfiguration "A pattern list must be given")

let comp_makefile_for_experiments experiment =
  List.flatten (comp_makefile_for_exp experiment)

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
  let named_cmdslist_exp = Setup.ExpTbl.fold
    (fun name_exp exp cmds-> (name_exp, comp_makefile_for_experiments exp)::cmds)
    Setup.experiments []
  in
  let cmdslist_exp = snd (List.split named_cmdslist_exp) in
  let cmdslist = cmdslist @ cmdslist_exp in
  let cmds = List.flatten cmdslist in
  let prjpatt = fst (List.split (cmds)) in
  let (fullprjs, fullpatts) = List.split prjpatt in
  let prjs = Misc.unique_list fullprjs in
  let patts = Misc.unique_list fullpatts in
    (
      List.map (compute_make_for_prj cmds) prjs,
      List.map (compute_make_for_patt cmds) patts,
      List.map compute_make_for_exp named_cmdslist_exp
    )

let init_env v1 v2 v3 configfile cvs =
  ignore(Config.parse_config configfile);
  [%info_log "Config parsing OK!"];
  Config.show_config ();
  let (prjs, patts, exps) = compute_makefile () in
  let err = check_setup v1 prjs patts in
  if err = [] then
    (
      gen_makefile v1 v2 prjs patts exps;
      erase_file v1 v2 prjs patts;
      if cvs then gen_cvsignore v1 prjs
    )
  else
    (
      [%error_log " *** KNOWN PROBLEMS ***"];
      List.iter (fun msg -> [%error_log msg]) err;
      [%error_log " *** END OF REPORT ***"]
    )
