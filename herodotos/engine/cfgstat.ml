exception Malformed of string

let count_bugs bugs =
  List.fold_left (fun (todo, bug, fp, other) (Ast_org.Org (_, s, _, _, _)) ->
		    match s with
			Ast_org.TODO -> (todo+1, bug, fp, other)
		      | Ast_org.BUG  -> (todo, bug+1, fp, other)
		      | Ast_org.FP   -> (todo, bug, fp+1, other)
		      | _            -> (todo, bug, fp, other+1)
		 ) (0,0,0,0) bugs

let count_correl correls =
  List.fold_left (fun (todo, same, unrelated, other) (s, _, _, _, _, _, _,_) ->
		    match s with
			Ast_org.TODO	  -> (todo+1, same, unrelated, other)
		      | Ast_org.SAME	  -> (todo, same+1, unrelated, other)
		      | Ast_org.UNRELATED -> (todo, same, unrelated+1, other)
		      | _                 -> (todo, same, unrelated, other+1)

		 ) (0,0,0,0) correls


let stat_patt_prj v1 v2 v3 cinfo fpinfo bugfile_ext =
  let file = Filename.chop_suffix (Filename.basename bugfile_ext) Global.bugext in
  let (p,patt) =
    match Str.split (Str.regexp_string Global.sep) file with
	[] -> raise (Malformed ("Malformed bug filename: "^bugfile_ext))
      | p::tail -> (p,String.concat Global.sep tail)
  in
  if Config.get_format patt = Ast_config.Org then
    let pdir = Config.get_prjdir p in
    let duple = Printf.sprintf ("%s 10s %s 16s") p patt in
    if (not ((Config.get_correl_mode v1 patt) = Ast_config.Nocorrel)) then
      try
	    let (depth, vlist) = Config.get_versinfos p in
	    let prefix = !Setup.projectsdir ^ pdir ^"/" in
	    let bugfile = Filename.chop_suffix bugfile_ext Global.bugext in
	      (* Should we get information about correlations *)
	    let (correl2,(todo, same, unrelated, other)) =
	      if cinfo then
		let correlfile = bugfile ^ Global.correlext in
		let correl = Org.parse_org false correlfile in
		let correl2 = Org.compute_correlation prefix depth correl in
		  (correl2, count_correl correl2)
	      else ([],(0,0,0,0))
	    in

	    (* Should we get information about bugs *)
	    let orgs = if cinfo && fpinfo
	      then
		let orgfile = file ^ Global.origext in
		Org.build_org_arr prefix depth !Setup.resultsdir pdir orgfile vlist
	      else Org.emptyarray vlist
	    in
	    let bugs =
	      if fpinfo && Sys.file_exists bugfile_ext
	      then Org.parse_org false bugfile_ext
	      else []
	    in
	      (*
		Compute correlation from correlation org file
		then process defect report using correlation information
	      *)
	    let (todo_bug, bug, fp, other_bug) = count_bugs bugs in
	    let count =
	      if v1 && cinfo && fpinfo then
		begin
		  let difffile = Diff_type.GNUDiff (bugfile ^ Global.patchext) in (* TODO: FIXME for gumtree *)
		  let diffs  = Diff.parse_diff v1 prefix difffile in
		  let strict = ((Config.get_correl_mode v1 patt) = Ast_config.Strict) in
		  let _ = if strict then (let logmsg=Printf.sprintf "*** INFO *** Strict correlation used for the pattern %s" patt in Bolt.Logger.log "" Bolt.Level.INFO logmsg) in
		  let emptyannots = Org.emptyarray vlist in
		  let cpucore = Setup.getCPUcore () in
		  let ((count,new_bugs), _) = Occ_correl.compute_org v3 cpucore strict prefix depth vlist diffs correl2 emptyannots orgs in
		    count
		end
	      else 0
	    in
	    let reports = if cinfo && fpinfo then Org.length orgs else 0 in
	    let correl =
	      if cinfo then
		Printf.sprintf "(T:% 5d S:% 5d U:% 5d O:% 5d)% 5d" todo same unrelated other (List.length correl2)
	      else ""
	    in
	    let fp = 
	      if fpinfo then
		begin
		  let bugcount = List.length bugs in
		  let fpratio = (float_of_int (fp*100)) /. (float_of_int bugcount) in
		  Printf.sprintf "(T:% 5d  B:% 5d F:% 5d [% 5.1f] O:% 5d)% 5d" todo_bug bug fp fpratio other_bug bugcount
		end
	      else ""
	    in
	      if cinfo && fpinfo then
		let auto =  if v1 then Printf.sprintf "% 6d" count else "?????" in
                let logmsg=Printf.sprintf "%s R:% 6d %s / %s -> %s" duple reports correl auto fp in
		Bolt.Logger.log "" Bolt.Level.INFO logmsg
	      else
		if cinfo then
                  let logmsg2=Printf.sprintf "%s %s" duple correl in
		  Bolt.Logger.log "" Bolt.Level.INFO logmsg2
		else
                  let logmsg2=Printf.sprintf "%s %s" duple fp in
		  Bolt.Logger.log "" Bolt.Level.INFO logmsg2
      with err ->
        let logmsg3=Printf.sprintf "%s Skipping..." duple in
	    Bolt.Logger.log "" Bolt.Level.ERROR logmsg3;
	    if !Misc.debug then raise err;
	    else
              let logmsg4=Printf.sprintf "%s (no correlation requested) Skipping..." duple in
	  Bolt.Logger.log "" Bolt.Level.ERROR logmsg4

let stats v1 v2 v3 configfile graph statmode =
  let correl = if statmode = Global.Stat || statmode = Global.Statcorrel then true else false in
  let fp =  if statmode = Global.Stat || statmode = Global.StatFP then true else false in
  (* If correl and fp are both set to false, we request both... and more ! *)
  ignore(Config.parse_config configfile);
  let logmsg=Printf.sprintf "Running in stat mode: correl stat %b / fp stat %b" correl fp in
  Bolt.Logger.log "" Bolt.Level.INFO logmsg;
  let bugfiles = Cfghelper.get_bugset graph in
  List.iter (stat_patt_prj v1 v2 v3 (correl || not fp) (fp || not correl)) bugfiles
