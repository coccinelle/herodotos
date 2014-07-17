
let show_opt opt =
  match opt with
      None -> "None"
    | Some _ -> "Some(_)"

let fast_bug prefix vlist bug =
  let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
    (f, t, [v], pos)
(*
  let enclosebug = (l+1, s, r, f, v, pos, face, t, n, sub) in
  let link = Org.make_orglink_of_bug prefix enclosebug in
  let enclose = Ast_org.Org(l+1, Ast_org.EMPTY, r, link, sub) in
  (l, s, r, f, v, pos, face, t, n, List.rev (enclose::(List.rev sub)))
*)

let fast_hash_bug prefix vlist (flist, tbl)=
  List.fold_left (fun head file ->
		    let (orglist:Ast_org.bug list) = Hashtbl.find tbl file in
		    let buglist = List.map (fast_bug prefix vlist) orglist in
		      head@buglist
		 ) [] flist

let get_bugs_of vlist orgs ver file = 
  try
    let vidx = Misc.get_idx_of_version vlist ver in
    Hashtbl.find (snd (Array.get orgs vidx)) file
  with Not_found -> []

let get_bugs_in_next_of vlist orgs ver file =
  try
    let vnidx = 1 + Misc.get_idx_of_version vlist ver in
    if vnidx < Array.length vlist then      (* Not a bug of the last version *)
      Hashtbl.find (snd (Array.get orgs vnidx)) file (* Get list of bugs in next version of file *)
    else
      []
  with Not_found -> []

let may_have_changed strict prefix vlist bfl bug =
  let (_, s, _, file, ver, pos, _, t, _, next, _) = bug in
  if next = {Ast_org.def = None} then LOG "may_have_changed: no next computed" LEVEL TRACE;
  next = {Ast_org.def = None}
  &&
    List.exists
    (fun (_, _, _, file2, ver2, pos2, _, t2, head, _, _) ->
      head.Ast_org.is_head = true
    ) (get_bugs_in_next_of vlist bfl ver file)

let find_disappeared strict prefix vlist (orgs:Ast_org.orgarray) : Ast_org.bugs =
  Array.fold_left (* Walk through version *)
    (fun head (flist, tbl) ->
      List.fold_left (* Files *)
	(fun fhead file ->
	  List.fold_left (* and bugs *)
	    (fun bhead bug ->
	      if may_have_changed strict prefix vlist orgs bug then
		bug::bhead
	      else
		bhead
	    ) fhead (Hashtbl.find tbl file)
	) head flist
    ) [] orgs

let is_SAME_as_next_in_correl correl file ver pos =
  LOG "is_SAME_as_next_in_correl: start - %s/%s" ver file LEVEL TRACE;
  let res =
    List.exists
      (fun (cs, _, _, _, cnfile, cnver, cnpos, _) ->
	cs = Ast_org.SAME
	&& cnfile = file
	  && cnver  = ver
	    && cnpos  = pos
      ) correl
  in
  LOG "is_SAME_as_next_in_correl: %s/%s %b" ver file res LEVEL TRACE;
  res

let find_all_next strict prefix vlist (orgsarray:Ast_org.orgarray) correl bug =
  let (_, _, _, file, ver, pos, _, t, _, _, _) = bug in
  let orgs = get_bugs_in_next_of vlist orgsarray ver file in
  let vn = Misc.get_next_version vlist ver in
  let (_, cb, ce) = pos in
  LOG "find_all_next: checking for %s/%s%s - %s"
    ver file (Org.get_string_pos pos) (Org.clean_link_text prefix ver file pos t) LEVEL TRACE;
   (*
      First, we find all bugs in next version with the same
      pattern size.
    *)
    List.find_all
      (fun next2 ->
	 let (_, _, _, file2, ver2, pos2, _, t2, _, _, _) = next2 in
	 let (_, cb2, ce2) = pos2 in
	   file = file2 && vn = ver2
	     (* Expression have at least the same length
		So, a buggy pattern should not be edited,
		but the line may have been edited.
	     *)
	     && (ce-cb) = (ce2-cb2)
	     && (if strict then
		   let new_t = Org.clean_link_text prefix ver file pos t in
		   let new_tb = Org.clean_link_text prefix ver2 file pos2 t2 in
		   LOG "find_all_next: \"%s\" <-> \"%s\"" new_t new_tb LEVEL TRACE;
		   new_t = new_tb
		 else true)
	     && not (is_SAME_as_next_in_correl correl file vn pos2)
	     (*
	       We then remove bugs already in relation with another one.
	     *)
	     && not (List.exists
		       (fun (_, _, _, pfile, pver, ppos, _, _, _, next, _) ->
			 LOG "find_all_next: check %s/%s%s def:%s" pver pfile (Org.get_string_pos ppos) (show_opt next.Ast_org.def) LEVEL TRACE;
			 next = {Ast_org.def = Some (Some next2)}
				&& not (file = pfile && ver = pver && pos = ppos)
		       )
		       orgs)
      ) orgs

let exists_bug_for_correl vlist orgs cbug =
  let (cs, cfile, cver, cpos, _, cnver, cnpos,_) = cbug in
  LOG "exists_bug_for_correl: check in %s from %s%s to %s%s"
    cfile
    cver (Org.get_string_pos cpos)
    cnver (Org.get_string_pos cnpos) LEVEL TRACE;
  List.exists (fun bug ->
    let (_, _, _, file, ver, pos, _, _, _, next, _) = bug in
    LOG "exists_bug_for_correl: %s def:%s" (Org.show_bug true bug) (show_opt next.Ast_org.def) LEVEL TRACE;
    cpos = pos
    && cs = Ast_org.SAME
  ) (get_bugs_of vlist orgs cver cfile)
 
let get_t_of_bug_for_next_correl strict prefix vlist orgs cbug =
  let (cs, _, _, _, cfile, cver, cpos, ct) = cbug in
  LOG "get_t_of_bug_for_next_correl: check for %s/%s%s" cver cfile (Org.get_string_pos cpos) LEVEL TRACE;
  let (_, _, _, _, _, _, _, t, _, _, _) =
    List.find (fun bug ->
      let (_, _, _, file, ver, pos, _, t, _, _, _) = bug in
      cpos = pos
    ) (get_bugs_of vlist orgs cver cfile)
  in
  let new_t = Org.clean_link_text prefix cver cfile cpos t in
  LOG "get_t_of_bug_for_next_correl: check \"%s\" <-> \"%s\"" ct new_t LEVEL TRACE;
  new_t

let get_all_correl strict prefix vlist correl orgs bug =
  let (_, _, _, file, ver, pos, _, bug_t, _, _, _) = bug in
  let new_tb = Org.clean_link_text prefix ver file pos bug_t in
  LOG "get_all_correl: start - %s - %s" (Org.show_bug true bug) new_tb LEVEL TRACE;
  List.find_all
    (fun cbug ->
      let (cs, cfile, cver, cpos, cnfile, cnver, cnpos, t) = cbug in
      cfile = file
      && cver = ver
	&& cpos = pos
	  && (if strict then
	      try
		let new_nt = get_t_of_bug_for_next_correl strict prefix vlist orgs cbug in
		let new_t = Org.clean_link_text prefix ver file pos t in
		LOG "get_all_correl: %s \"%s\" <-> \"%s\"" (Org.show_bug true bug) new_t new_nt LEVEL TRACE;
		new_t = new_tb && new_t = new_nt
	      with Not_found ->
		LOG "get_all_correl: Not_found raised while checking for %s/%s%s" ver file (Org.get_string_pos pos) LEVEL WARN;
		false
	    else true)
    ) correl

let gen_todo ch strict vlist prefix orgs correl bug =
  (* TODO : Also propose correlation when basename is identical *)
  (*
    For remaining disappearing bugs, propose correlation with
    every available bug in next.
  *)
  LOG "gen_todo called for %s" (Org.show_bug true bug) LEVEL TRACE;
  let next = find_all_next strict prefix vlist orgs correl bug in
  List.iter (fun nbug ->
	       Printf.fprintf ch "* TODO %s\n %s\n"
		 (Org.make_orglinkbug true prefix bug)
		 (Org.make_orglinkbug true prefix nbug)
	    )
    next;
    List.length next

let correlate verbose strict prefix vlist correlfile prefix rev_correl orgsarray orgs =
  (try
     Unix.rename correlfile (correlfile^".bk");
   with _ -> ()
  );
  let ch = open_out correlfile in
  let correl = Misc.unique_list (List.rev rev_correl) in
  (* let buglist = List.flatten (List.map (fun x -> List.flatten (snd x)) orgs) in *)
  let disps = find_disappeared strict prefix vlist orgsarray in
  LOG "------------------" LEVEL TRACE;
  Org.show_correlation true correl;
  LOG "------------------" LEVEL TRACE;
  Org.show_buglist true disps;
  LOG "------------------" LEVEL TRACE;
  let todo = List.map (fun bug ->
    LOG "correlate: Start mapping..." LEVEL TRACE;
    try
      (*
	For bugs that still disappear, we keep the UNRELATED info.
	It should not have SAME correlation here because SAME
	implies a correlation and thus the bug doesn't disappear
	anymore ! :)
	Useless SAME are filtered above.
      *)
      let bug_correl = get_all_correl strict prefix vlist correl orgsarray bug in
      if bug_correl = [] then
	(*
	  We don't know anything from previous execution.
	  But we may propose some TODO.
	*)
	gen_todo ch strict vlist prefix orgsarray correl bug
      else
	let todo = List.map
	  (fun (s, file, ver, pos, nfile, nver, npos, t) ->
	    LOG "correlate: Start nested map" LEVEL TRACE;
	    let nbug =
	      (max_int, s, "", nfile, nver, npos, "ovl-face1", "", {Ast_org.is_head = false}, {Ast_org.def = None},[]) in
	    if s = Ast_org.TODO then
	      begin
		if not (is_SAME_as_next_in_correl correl file nver npos)
		then
		  (Printf.fprintf ch "* TODO %s\n %s\n"
		     (Org.make_orglinkbug true prefix bug)
		     (Org.make_orglinkbug true prefix nbug);
		   true)
		else
		  false
	      end
	    else
	      begin
		let status = Org_helper.get_status s in
		Printf.fprintf ch "* %s %s\n %s\n"
		  status
		  (Org.make_orglinkbug true prefix bug)
		  (Org.make_orglinkbug true prefix nbug);
		false
	      end
		
	  ) bug_correl in
	List.length (List.filter (fun x -> x) todo)
    with e ->
      LOG "EXN %s" (Printexc.to_string e) LEVEL FATAL;
      Debug.trace (Printexc.get_backtrace ());
      gen_todo ch strict vlist prefix orgsarray correl bug
  ) disps in
    List.iter (fun  cbug ->
      let (s, file, ver, pos, nfile, nver, npos, t) = cbug in
      let bug = (1, s, "", file, ver, pos, "ovl-face1", t, {Ast_org.is_head=true},{Ast_org.def = None},[]) in
      let nbug =
	(max_int, s, "", nfile, nver, npos, "ovl-face1", "", {Ast_org.is_head=false}, {Ast_org.def = None},[]) in
      if s = Ast_org.SAME then
	if exists_bug_for_correl vlist orgsarray cbug then
	  (*
	    We keep SAME correlation information
	    but prune by default others.
	  *)
	  Printf.fprintf ch "* SAME %s\n %s\n"
	    (Org.make_orglinkbug true prefix bug)
	    (Org.make_orglinkbug true prefix nbug)
	else
	  LOG "*** INFO *** Drop an old SAME entry of a now non-existing bug: %s" (Org.show_bug true bug) LEVEL INFO
    ) correl;
    Printf.fprintf ch "\n* org config\n#+SEQ_TODO: TODO | SAME UNRELATED\n";
    close_out ch;
    List.fold_left (+) 0 todo (* Count the number of TODO generated *)
