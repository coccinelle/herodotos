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
	(*let (pdir, file_ext) = Misc.strip_prefix (!Setup.resultsdir^"/") bugfile_ext in
	  let pfile = (Filename.chop_suffix file_ext Global.bugext) ^ Global.cocciext in
	  let p = Config.rev_prjdir pdir in
	  let patt = Config.rev_pattfile pfile in
	*)
      let duple = Printf.sprintf "%- 10s %- 16s" p patt in
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
	      let orgfile = bugfile ^ Global.origext in
		Org.format_orgs_to_arr prefix depth vlist (Org.parse_org false orgfile)
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
		  let _ = if strict then print_endline ("*** INFO *** Strict correlation used for the pattern "^patt) in
		  let emptyannots = Org.emptyarray vlist in
		  let cpucore = Setup.getCPUcore () in
		  let ((count,new_bugs), _) = Occ_correl.compute_org v3 cpucore strict prefix depth vlist diffs correl2 emptyannots orgs in
		    count
		end
	      else 0
	    in
	      print_string duple;
	      if cinfo && fpinfo then print_string (" R:" ^ (Printf.sprintf "% 6d" (Org.length orgs)));
	      if cinfo then
		begin
		  print_string (" (T:" ^ (Printf.sprintf "% 5d" todo));
		  print_string (" S:" ^ (Printf.sprintf "% 5d" same));
		  print_string (" U:" ^ (Printf.sprintf "% 5d" unrelated));
		  print_string (" O:" ^ (Printf.sprintf "% 5d" other));
		  print_string (")" ^ (Printf.sprintf "% 5d" (List.length correl2)))
		end;
	      if cinfo && fpinfo then
		begin
		  if v1 then print_string (" /" ^ (Printf.sprintf "% 6d" count))
		  else print_string (" / ?????");
		  print_string (" -> ");
		end;
	      if fpinfo then
		begin
		  let bugcount = List.length bugs in
		  let fpratio = (float_of_int (fp*100)) /. (float_of_int bugcount) in
		    print_string ("(T:" ^ (Printf.sprintf "% 5d" todo_bug));
		    print_string (" B:" ^ (Printf.sprintf "% 5d" bug));
		    print_string (" F:" ^ (Printf.sprintf "% 5d [% 5.1f]" fp fpratio));
		    print_string (" O:" ^ (Printf.sprintf "% 5d" other_bug));
		    print_string (")" ^ (Printf.sprintf "% 5d" bugcount));
		end;
	      print_newline ()
		(*    ;Org.auto_correl := 0 *)
	  with err ->
	    print_endline (duple ^" Skipping...");
	    if !Misc.debug then raise err;
	else
	  print_endline (duple ^" (no correlation requested) Skipping...")

let stats v1 v2 v3 configfile graph statmode =
  let correl = if statmode = Global.Stat || statmode = Global.Statcorrel then true else false in
  let fp =  if statmode = Global.Stat || statmode = Global.StatFP then true else false in
  (* If correl and fp are both set to false, we request both... and more ! *)
  ignore(Config.parse_config configfile);
  let bugfiles = Cfghelper.get_bugset graph in
    List.iter (stat_patt_prj v1 v2 v3 (correl || not fp) (fp || not correl)) bugfiles
