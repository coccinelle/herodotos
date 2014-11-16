exception Malformed of string

(* TO REMOVE ?

let print_org org = let Ast_org.Org(i , status , text , link , orgs)= org in let (path,o,t) = link in
                    Printf.printf "%d | %s | %s | %s | %s  \n" i (Org_helper.get_status status) text path t


let print_bug bug = let (ind , status , tex , path , vname , pos , str , text , is_head , next , orgs) = bug in let (l,cb,ce) = pos in
                    Printf.printf "%d | %s | %s | %s| %s | (%d,%d,%d) %s\n " ind (Org_helper.get_status status) tex path vname l cb ce text ;
                    List.iter(fun o->print_org o) orgs
 

let print_bugs2 bugs2 = List.iter(fun e->let (p,bl)=e in Printf.printf "PATH %s\n" p;List.iter(fun bugs-> List.iter(fun bug->print_bug bug )bugs)bl)bugs2
*)

let rec count_todo ch =
  try
    let line = input_line ch in
    let re = Str.regexp "^\\* TODO" in
      if Str.string_match re line 0 then
	(count_todo ch) + 1
      else
	count_todo ch
  with _ -> 0

let map_to_size bugfile_ext =
  let bugfile = Filename.chop_suffix bugfile_ext Global.bugext in
  let orgfile = bugfile ^ Global.origext in
    if Sys.file_exists orgfile then
      let ch = open_in orgfile in
      let todo = count_todo ch in
	close_in ch;
	(todo, bugfile_ext)
    else
	(0, bugfile_ext)

let clean_file exist editfile =
  if Sys.file_exists editfile then
    begin
      if exist then LOG "*** INFO *** %s is thus removed." editfile LEVEL INFO;
      Sys.remove editfile
    end

let write_org outputfile prefix orgs =
  let ch = open_out outputfile  in
    Org.print_bugs ch prefix orgs;
    close_out ch

let reparse_org tmpfile prefix depth vlist orgs =
(*   let toporgs = List.flatten (snd (List.split orgs)) in *)
(*   let editorgs = List.map (Bugs.wrap_bugs prefix) toporgs in *)
(*  let tmpfile = Filename.temp_file "" "" in *)
  let _ = write_org tmpfile prefix orgs in
  let cleanorgs =
    Org.format_orgs_to_arr prefix depth vlist (Org.parse_org false tmpfile)
  in
(*    Sys.remove tmpfile; *)
    Org.sort cleanorgs;
    cleanorgs

let correl_patt_prj v1 v2 v3 cpucore diffalgo bugfile_ext =
  let file = Filename.chop_suffix (Filename.basename bugfile_ext) Global.bugext in
  let (p,patt) =                                                                                                                    
    (* TODO: Fixme. This may not work according to the names of the projects *)
    match Str.split (Str.regexp_string Global.sep) file with  
	[] -> raise (Malformed ("Malformed bug filename: "^bugfile_ext))
      | p::tail -> (p,String.concat Global.sep tail) 
  in
  let pdir = Config.get_prjdir p in 
  let (depth, vlist) = Config.get_versinfos p in
  let prefix = !Setup.projectsdir ^ pdir ^"/" in  
  let bugfile = Filename.chop_suffix bugfile_ext Global.bugext in
  let orgfile    = bugfile ^ Global.origext in
  let correlfile = bugfile ^ Global.correlext in
  let difffile   = Diff.select_diff diffalgo p in
  let editfile   = bugfile ^ Global.editext in
    if Config.get_format patt = Ast_config.Org then
	if (not ((Config.get_correl_mode v1 patt) = Ast_config.Nocorrel)) then
	  begin
            let orgs1 = (Org.build_org_arr prefix depth !Setup.resultsdir pdir (file^Global.origext) vlist ) in
	    let diffs1= Diff.get_diff v1 cpucore !Setup.resultsdir pdir prefix vlist orgs1 (file^Global.origext) difffile in
	    let correl = List.rev (Org.parse_org false correlfile) in
	      (* 
		Compute correlation from correlation org file
		then process defect report using correlation information
	      *)
	    let correl2 = Org.compute_correlation prefix depth correl in  
	    let annots =
 	      if Sys.file_exists bugfile_ext
	      then Org.format_orgs_to_arr prefix depth vlist (Org.parse_org false bugfile_ext)
	      else Org.emptyarray vlist
	    in
	    let strict = ((Config.get_correl_mode v1 patt) = Ast_config.Strict) in
	    let ((count,new_bugs), orgs2) = Occ_correl.compute_org v3 cpucore strict prefix depth vlist diffs1 correl2 annots orgs1 in 
	    let todos = Correl.correlate v1 strict prefix vlist correlfile prefix correl2 orgs1 orgs2 in
	    let ccount =  List.length correl2 in
	    let todostr =
	      if todos <> 0
	      then " *** "^correlfile^" ***"
	      else ""
	    in
	    let bug_msg =
	      if new_bugs = 0
	      then ""
	      else "" (* " "^string_of_int new_bugs^ " new REPORTS (potential BUGS)." *)
	    in
	    let msg = Printf.sprintf "%- 10s %- 16s\t% 5d / % 6d (% 5d TODO)%s%s" p patt ccount count todos todostr bug_msg in
	    LOG msg LEVEL INFO;
	    if !Misc.debug then
	      (Diff.show_diff v3 vlist diffs1;
	       Org.show_org v2 prefix orgs2);
	    if todos = 0 then
	      if not (Sys.file_exists bugfile_ext)
	      then (write_org bugfile_ext prefix orgs2; (*edition du .new.org *)
		    LOG "*** NEW FILE TO EDIT *** %s" bugfile_ext LEVEL INFO)  
	      else
		let exist = Sys.file_exists editfile in
		let cleanorgs = reparse_org editfile prefix depth vlist orgs2 in
		Org.sort annots;
 		if not (cleanorgs = annots) then
		  LOG "*** NEW FILE TO EDIT *** %s" editfile LEVEL INFO
		else
		  begin
		    LOG "*** INFO *** %s is up to date." bugfile_ext LEVEL INFO;
		    if not !Misc.debug
		    then clean_file exist editfile
		    else
		      begin
			let ch = open_out editfile in
			let cleanlist = Org.list_of_bug_array annots in
			Org.print_bugs_raw ch prefix cleanlist;
			LOG "*** INFO *** %s contains a sorted version." editfile LEVEL INFO;
		      end
		  end
	    else
	      if !Misc.debug then
		write_org editfile prefix orgs2
	  end
	else
	  LOG "*** NO CORREL *** %s" orgfile LEVEL INFO
    else
      if Config.get_format patt = Ast_config.Org then
	LOG "*** SKIP (NOT FOUND) *** %s" orgfile LEVEL WARN

let correl_patt_prj_nofail v1 v2 v3 cpucore diffalgo file =
  try
    correl_patt_prj v1 v2 v3 cpucore diffalgo file;
    0
  with Config.Warning msg ->
    LOG "*** WARNING *** %s" msg LEVEL WARN;
    1

let run_correl_job v1 v2 v3 cpucore diffalgo file =
  let pid = Unix.fork () in
    if pid = 0 then (* I'm a slave *)
      begin
	(if !Misc.debug then
	    let pid = Unix.getpid() in
	    LOG "New child %d on %s" pid file LEVEL TRACE
	 else
	    LOG "New child on %s" file LEVEL TRACE);
	let ret = correl_patt_prj_nofail v1 v2 v3 cpucore diffalgo file in
	(if !Misc.debug then
	    LOG "Job done for child %d - exit %d" pid ret LEVEL TRACE
	 else
	    LOG "Job done for child - exit %d" ret LEVEL TRACE
	);
	let msg = Debug.profile_diagnostic () in
	if msg <> "" then
	  Debug.trace msg;
	exit ret
      end
    else (* I'm the master *)
      pid

let dispatch_correl_job v1 v2 v3 cpucore diffalgo (perr, pidlist) file :int*int list =
  let (error, newlist) =
    if List.length pidlist > cpucore then
      let (death, status) = Unix.wait () in
      (if !Misc.debug then
	  LOG "Master: Job done for child %d" death LEVEL TRACE
       else
	  LOG "Master: Job done for child" LEVEL TRACE
      );
      let error = match status with
	    Unix.WEXITED 0 -> perr
	  | _              -> perr + 1
	in
	(error, List.filter (fun x -> x <> death) pidlist)
    else
      (perr, pidlist)
  in
  let pid = run_correl_job v1 v2 v3 cpucore diffalgo file in
    (error, pid::newlist)

let correl v1 v2 v3 configfile diffalgo filter =
  ignore(Config.parse_config configfile);
  LOG "Config parsing OK!" LEVEL INFO;
  Config.show_config ();
  let unorder_bugfiles = (Cfghelper.get_bugset_gen filter) in
  let size_of = List.map map_to_size unorder_bugfiles in   
  let order = List.sort (fun a b -> -(compare (fst a) (fst b))) size_of in
  List.iter (fun (s, n) -> LOG "%d %s" s n LEVEL DEBUG) order;
  let bugfiles = snd (List.split order) in 
  let cpucore = Setup.getCPUcore () in
  let error =
    if cpucore = 1 then
      let errs = List.map (correl_patt_prj_nofail v1 v2 v3 cpucore diffalgo)  bugfiles in
	List.fold_left (+) 0 errs
    else
      let (err, pidlist) = List.fold_left (dispatch_correl_job v1 v2 v3 cpucore diffalgo) (0, []) bugfiles in
      let res = List.map (fun x ->
	let (death, status) = Unix.wait () in
	(if !Misc.debug then
	    LOG "Master: Job done for child %d" death LEVEL TRACE
	 else
	    LOG "Master: Job done for child" LEVEL TRACE
	);
	match status with
	    Unix.WEXITED 0 -> 0
	  | _ -> 1
      ) pidlist
      in
	List.fold_left (+) err res
  in
    if error <> 0 then
      LOG ("*** ERROR *** %d error(s) during the correlation.") error LEVEL ERROR




