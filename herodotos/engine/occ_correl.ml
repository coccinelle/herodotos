let match_bug strict prefix bug1 bug2 =
  let (_, s, _, f, v, pos, _, t, h, _, _) = bug1 in
  let (_, sb, _, fb, vb, posb, _, tb, hb, _, _) = bug2 in
    f = fb && v = vb && pos = posb
(*       && h.Ast_org.is_head = true *)
(*       && hb.Ast_org.is_head = true *)
      && (if strict then
	    let new_t = Org.clean_link_text prefix v f pos t in
	    let new_tb = Org.clean_link_text prefix v f pos tb in
            let logmsg=Printf.sprintf "match_bug: \"%s\" <-> \"%s\"" new_t new_tb in
	    Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
	    new_t = new_tb
	  else true)

let rec get_chain (bug:Ast_org.bug) =
  let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
    match n.Ast_org.def with
	None -> [bug]
      | Some (None) -> [bug]
      | Some (Some (nbug, _)) -> bug::get_chain nbug

let is_head bug =
  let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
    h.Ast_org.is_head

let extract_chain ext_file (bugs: Ast_org.orgarray) : Ast_org.bugs list =
  let heads =
    Array.fold_left
      (fun head (flist, tbl) ->
	 List.fold_left
	   (fun fhead file ->
	      if ext_file = file then
		List.fold_left
		  (fun bhead bug ->
		     if is_head bug then
		       (get_chain bug)::bhead
		     else
		       bhead
		  ) fhead (Hashtbl.find tbl file)
	      else
		fhead
	   ) head flist
      ) [] bugs
  in
    heads

let get_bug strict prefix (bug: Ast_org.bug) (bugs: Ast_org.bug list) : Ast_org.bug =
  List.find (match_bug strict prefix bug) bugs

let get_next_list strict prefix vlist bugs f vidx old_t =
  let (_, tbl) = Array.get bugs (vidx+1) in
  try
    let buglist = Hashtbl.find tbl f in
    List.filter (fun bug ->
      let (l, s, r, f, v, p, face, t, h, n, _) = bug in
      h.Ast_org.is_head = true
      && if strict then
	  let t = Org.clean_link_text prefix v f p t in
	  old_t = t
	else true
    ) buglist
  with Not_found -> []

let update_nohead bug =
  let (l, s, r, f, v, p, face, t, h, n, subs) = bug in
  h.Ast_org.is_head <- false

let manual_check_next verbose strict prefix correl subbugs bug =
  let (l, s, r, f, v, p, face, t, h, n, _) = bug in
  let t = if strict then (Org.clean_link_text prefix v f p t) else "" in
  let logmsg=Printf.sprintf "Trying manual correlation of %s ver. %s" f v in
  Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
  try
    let correlb = List.find (fun b ->
      let (st, file, ver, pos, _, _, _, ctext) = b in
      st = Ast_org.SAME
      && f = file
	&& v = ver
	  && p = pos
	    && (if strict
	      then
		let new_t = Org.clean_link_text prefix v f pos ctext in
                let logmsg=Printf.sprintf "manual_check_next: \"%s\" <-> \"%s\"" new_t t in
		Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
		new_t = t
	      else true
	    )
    ) correl in
    let (_, _, _, _, nfile, nver, next_pos, _) = correlb in
    let check = (l, s, r, nfile, nver, next_pos, "", t, {Ast_org.is_head=true}, {Ast_org.def=None}, []) in
    (try
       let next = get_bug strict prefix check subbugs in
       n.Ast_org.def <- Some (Some (next, false));
       update_nohead next;
       Bolt.Logger.log "" Bolt.Level.TRACE "Manual correlation OK";
       Bolt.Logger.log "" Bolt.Level.TRACE "=========";
       0 (* No automatic correlation performed *)
     with Not_found ->
       Bolt.Logger.log "" Bolt.Level.TRACE "Manual correlation KO";
       Bolt.Logger.log "" Bolt.Level.TRACE "=========";
       0 (* No automatic correlation performed *)
    )
  with Not_found ->
    Bolt.Logger.log "" Bolt.Level.TRACE "Manual correlation KO - KO";
    Bolt.Logger.log "" Bolt.Level.TRACE "=========";
    0 (* No automatic correlation performed *)

let rec check_alt_next verbose strict prefix depth vlist diffs correl bugs bug : int =
  let (l, s, r, f, v, p, face, t, h, n, _) = bug in
  let t = if strict then (Org.clean_link_text prefix v f p t) else "" in
  let vidx = Misc.get_idx_of_version vlist v in
  let subbugs = get_next_list strict prefix vlist bugs f vidx t in
  match Diff.alt_new_pos diffs f v p with
      None ->
	Bolt.Logger.log "" Bolt.Level.TRACE "No alternative position";
	manual_check_next verbose strict prefix correl subbugs bug
    | Some (_, diffcheck_pos) ->
      match diffcheck_pos with
	  (Ast_diff.Sing line,colb,cole) ->
	    let (res, ks) = check_next verbose strict true prefix depth vlist diffs correl bugs bug (line,colb,cole) in
	    if ks <> None then Bolt.Logger.log "" Bolt.Level.FATAL "There is a continuation to run after alternative method have been called!";
	    if res = 1 && n.Ast_org.def = None then Bolt.Logger.log "" Bolt.Level.FATAL "check_next reports a success but no next is set!";
	    if n.Ast_org.def = None then
	      manual_check_next verbose strict prefix correl subbugs bug
	    else
	      1 (* Automatic correlation performed *)
	| (Ast_diff.Deleted _, colb,cole) ->
	  Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	  Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	  n.Ast_org.def <- Some (None);
	  1 (* Considered as an automatic correlation *)
	| (Ast_diff.Unlink, _, _) -> (* File has been removed. *)
	  Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	  Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	  n.Ast_org.def <- Some (None);
	  1 (* Considered as an automatic correlation *)
	| (Ast_diff.Cpl (lineb,linee),colb, cole) ->
	  if true then
	    manual_check_next verbose strict prefix correl subbugs bug
	  else (* TODO: See later if there is something to do here... *)
	    let rec fold line =
	      let (res, ks) = check_next verbose strict true prefix depth vlist diffs correl bugs bug (line,colb,cole) in
	      if res = 0 then        (* Nothing at line 'line' *)
		if line < linee then (* Check next lines until 'linee' *)
		  fold (line+1)
		else (0, ks)
	      else (1, None)           (* Found something. Stop there. *)
	    in
	    let (res, ks) = fold lineb in (* Start looking for next bug at line 'lineb' *)
	    if ks <> None then Bolt.Logger.log "" Bolt.Level.FATAL "There is a continuation to run after alternative method have been called!";
	    if res = 1 && n.Ast_org.def = None then Bolt.Logger.log "" Bolt.Level.FATAL "check_next reports a success but no next is set!";
	    if n.Ast_org.def = None then
	      manual_check_next verbose strict prefix correl subbugs bug
	    else
	      1 (* Automatic correlation performed *)

and check_next verbose strict conf prefix depth vlist diffs correl (bugs:Ast_org.orgarray) (bug:Ast_org.bug) check_pos
    : int * (string option * ((Ast_org.bug -> int) * Ast_org.bug)) option =
  Debug.profile_code_silent "check_next"
    (fun () ->
      let (l, s, r, f, v, p, face, t, h, n, _) = bug in
      let t = if strict then (Org.clean_link_text prefix v f p t) else "" in
      let vidx = Misc.get_idx_of_version vlist v in
      let subbugs = get_next_list strict prefix vlist bugs f vidx t in
      let vn = Misc.get_version_name vlist (vidx+1) in
      let check = (l, s, r, f, vn, check_pos, face, t, {Ast_org.is_head=true}, {Ast_org.def=None}, []) in
      let logmsg=Printf.sprintf "check_next of %s at %s" (Org.show_bug true bug) (Org.show_bug true check) in
      Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
      Bolt.Logger.log "" Bolt.Level.TRACE "List:";
      List.iter (fun bug -> (let logmsg=Printf.sprintf "%s" (Org.show_bug true bug) in Bolt.Logger.log "" Bolt.Level.TRACE logmsg)) subbugs;
      Bolt.Logger.log "" Bolt.Level.TRACE "";
      if subbugs = [] then
	begin
	  Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	  Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	  n.Ast_org.def <- Some (None);
	  (1, None) (* Automatic correlation performed *)
	end
      else
	try
	  let next = get_bug strict prefix check subbugs in
	  if (n.Ast_org.def = None) then
	    begin
	      Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	      Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	      n.Ast_org.def <- Some (Some (next, true));
	      update_nohead next;
	      (1, None) (* One automatic correlation performed *)
	    end
	  else
	    failwith "Already defined next !" (* No automatic correlation performed *)
	with Not_found ->
	  if conf then
	    begin
	      Bolt.Logger.log "" Bolt.Level.WARN "No next found, but confidence set";
	      Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	      Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	      n.Ast_org.def <- Some (None);
	      (1, None) (* Automatic correlation performed *)
	    end
	  else
	    begin
	      Bolt.Logger.log "" Bolt.Level.TRACE "Will try with an alternative method...";
	      Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	      (0, Some (Hybrid.get_cmd2 f v, (check_alt_next verbose strict prefix depth vlist diffs correl bugs, bug)))
	    end
    )

let compute_bug_next verbose strict prefix depth vlist diffs correl bugs bug =
  Debug.profile_code_silent "compute_bug_next"
    (fun () ->
       let (l, s, r, f, v, p, face, t, h, n, _) = bug in
       let t = if strict then (Org.clean_link_text prefix v f p t) else "" in
       let logmsg=Printf.sprintf "%s" (Org.show_bug true bug) in 
       Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
       (* Short path when there is no bug in next version *)
       let vidx = Misc.get_idx_of_version vlist v in
       let subbugs = get_next_list strict prefix vlist bugs f vidx t in
       if subbugs = [] then
	 begin
           let logmsg=Printf.sprintf "No bug in version %s of %s. Skip." (Misc.get_next_version vlist v) f in
	   Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
	   Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	   n.Ast_org.def <- Some (None);
	   (1, None) (* Considered as an automatic correlation *)
	 end
       else
	 (* Normal path *)
	 let (conf, diffcheck_pos) = Diff.compute_new_pos diffs f v p in
         let logmsg=Printf.sprintf "%s as new pos: %s" (Org.get_string_pos p) (Org.get_string_new_pos diffcheck_pos) in
	 Bolt.Logger.log ""
	   Bolt.Level.TRACE logmsg;
	 match diffcheck_pos with
	     (Ast_diff.Sing line,colb,cole) -> (* We are between two hunks. *)
	       check_next verbose strict conf prefix depth vlist diffs correl bugs bug (line,colb,cole)
	   | (Ast_diff.Deleted cont, colb, cole) ->
	     if not conf then
	     (*
	       We are inside a hunk and lines have been removed.
	       Check for manual correlation.
	     *)
	       check_next verbose strict conf prefix depth vlist diffs correl bugs bug (0,colb,cole)
	     else
	       if cont then
		 (* Considered as uncorrelated. Continuation will check with the alternative algorithm. *)
		 begin
		   Bolt.Logger.log "" Bolt.Level.TRACE "Will try with an alternative method...";
		   Bolt.Logger.log "" Bolt.Level.TRACE "=========";
		   (0, Some (Hybrid.get_cmd2 f v, (check_alt_next verbose strict prefix depth vlist diffs correl bugs, bug)))
		 end
	       else
		 begin
		   Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
		   Bolt.Logger.log "" Bolt.Level.TRACE "=========";
		   n.Ast_org.def <- Some (None);
		   (1, None) (* Considered as an automatic correlation *)
		 end
	   | (Ast_diff.Unlink, _, _) ->
	   (*
	     File has been removed.
	   *)
	     Bolt.Logger.log "" Bolt.Level.TRACE "Automatic correlation OK";
	     Bolt.Logger.log "" Bolt.Level.TRACE "=========";
	     n.Ast_org.def <- Some (None);
	     (1, None) (* Considered as an automatic correlation *)
	   | (Ast_diff.Cpl (lineb,linee),colb, cole) -> (* We are inside a hunk. *)
	     if true then
	       (0, Some (Hybrid.get_cmd2 f v, (check_alt_next verbose strict prefix depth vlist diffs correl bugs, bug)))
	     else  (* TODO: See later if there is something to do here... *)
	       (*
		 Could we do something for bugs inside a hunk ?
		 It seems to be impossible and worthless.
		 Just check for manual correlation for pure GNU Diff.
		 Try GumTree with the hybrid strategy.
	       *)
	     let rec fold line =
	       let (res, ks) = check_next verbose strict conf prefix depth vlist diffs correl bugs bug (line,colb,cole) in
	       if res = 0 then
		 if line < linee then
		   fold (line+1)
		 else (0, ks)
	       else (1, None)
	     in
	     let (res, ks) = fold lineb in (* Start looking for next bug at line 'lineb' *)
	     (* Check ks is empty, run manual otherwise *)
	     if res = 1 && n.Ast_org.def = None then Bolt.Logger.log "" Bolt.Level.FATAL "check_next reports a success but no next is set!";
	     if n.Ast_org.def = None then
		 (0, Some (Hybrid.get_cmd2 f v, (check_alt_next verbose strict prefix depth vlist diffs correl bugs, bug)))
	     else
	       (1, None) (* Automatic correlation performed *)
  )

let compute_bug_chain verbose strict prefix depth count vlist diffs correl bugs =
  Debug.profile_code_silent "compute_bug_chain"
    (fun () ->
      let bound = ((Array.length vlist) - 1) in
      snd (
	Array.fold_left
	  (fun (i, acc) (flist, tbl) ->
	    if i < bound then (* Skip last version - there is **no next bug** to match against ! *)
	      (i+1,
	       begin
		 List.fold_left
		   (fun acc file ->
		     let subbugs = Hashtbl.find tbl file in
		     (**)
		     List.fold_left
		       (fun (res_acc, ks_acc) bug ->
			 let (res, ksopt) = compute_bug_next verbose strict prefix depth vlist diffs correl bugs bug in
			 match ksopt with
			     None -> (res_acc + res, ks_acc)
			   | Some ks -> (res_acc + res, ks::ks_acc)
		       ) acc subbugs
		   (**)
		   ) acc flist
	       end
	      )
	    else
	      let (res, ks) = acc in
	      let last_report_count = List.fold_left
		(fun acc file -> acc + List.length (Hashtbl.find tbl file))
		res flist
	      in
	      (i+1, (last_report_count, ks))
	  ) (0, (0, [])) bugs
      )
    )

let find_all_org_in org orgs =
  let (_, so, _, fo, vo, po, _, to_, _, _, _) = org in
    List.find_all (fun (_, s, _, f, v, p, _, t, _, _, _) ->
		     f = fo && v = vo && p = po
		  ) orgs

let update_status vlist annots (bugs:Ast_org.bugs) : int * Ast_org.bugs =
  Debug.profile_code_silent "update_status"
    (fun () ->
       let (stats, res) =
	 List.split
	   (List.map
	      (fun bug ->
		 let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
		 let (new_bug, new_s, new_r, new_t) =
		   let idx = Misc.get_idx_of_version vlist v in
		   let (_,tbl) = Array.get annots idx in
		     try
		       match find_all_org_in bug (Hashtbl.find tbl f) with
			   [] -> (true, s, r, t)
			 | [new_] ->
			     let (_, new_s, new_r, _, _, _, _, new_t, _, _, _) = new_ in
			       (false, new_s, new_r, new_t)
			 | matching -> (false, s, r, t)
		     with Not_found ->
		       (true, s, r, t)
		 in
		   (new_bug, (l, new_s, new_r, f, v, pos, face, new_t, h, n, sub))
	      ) bugs
	   )
       in
	 (List.length (List.filter (fun x -> x) stats), res)
    )

let unique_file_list orgarray =
  Debug.profile_code_silent "unique_file_list"
    (fun () ->
       let file_list = Array.fold_left (fun head (flist, _) -> head @ flist) [] orgarray in
	 Misc.unique_list file_list
    )

let run_pariter f cmd : int =
  let pid = Unix.fork () in
    if pid = 0 then (* I'm a slave *)
      begin
	(if !Misc.debug then
           let logmsg=Printf.sprintf "New child %d for %s" (Unix.getpid ()) cmd in
	    Bolt.Logger.log "" Bolt.Level.TRACE logmsg
	 else
           let logmsg=Printf.sprintf "New child for %s" cmd in
	    Bolt.Logger.log "" Bolt.Level.TRACE logmsg
	);
	let status = f cmd in
	(if !Misc.debug then
           (let logmsg=Printf.sprintf "Job done for child %d" (Unix.getpid ()) in
	    Bolt.Logger.log "" Bolt.Level.TRACE logmsg)
	 else
	    Bolt.Logger.log "" Bolt.Level.TRACE "Job done for child"
	 );
	let msg = Debug.profile_diagnostic () in
	if msg <> "" then Debug.trace msg;
	exit status
      end
    else (* I'm the master *)
      pid

let dispatch_pariter cpucore f (perr, pidlist) cmd : int * int list =
  let (error, newlist) =
    if List.length pidlist > cpucore then
      let (death, status) = Unix.wait () in
      (if !Misc.debug then
         let logmsg=Printf.sprintf "Master: Job done for child %d" death in
	  Bolt.Logger.log "" Bolt.Level.TRACE logmsg
       else
	  Bolt.Logger.log "" Bolt.Level.TRACE "Master: Job done for child"
      );
      let error = match status with
	  Unix.WEXITED 0 -> perr
	| _              -> perr + 1
      in
      (error, List.filter (fun x -> x <> death) pidlist)
    else
      (perr, pidlist)
  in
  let pid = run_pariter f cmd in
  (error, pid::newlist)

let pariter cpucore f cmds : unit =
  Debug.profile_code_silent "pariter"
    (fun () ->
      let error =
	if cpucore = 1 then
	  (List.iter (fun x -> ignore (f x)) cmds;  0)
	else
	  let (err, pidlist) =
	    List.fold_left
	      (dispatch_pariter cpucore f)
	      (0, [])
	      cmds
	  in
	  let res = List.map (fun x ->
	    let (death, status) = Unix.wait () in
	    (if !Misc.debug then
               let logmsg=Printf.sprintf "Master: Job done for child %d" death in
		Bolt.Logger.log "" Bolt.Level.TRACE logmsg
	     else
		Bolt.Logger.log "" Bolt.Level.TRACE "Master: Job done for child"
	    );
	    match status with
		Unix.WEXITED 0 -> 0
	      | _ -> 1
	  ) pidlist
	  in
	  List.fold_left (+) err res
      in
      if error <> 0 then
        let logmsg=Printf.sprintf "*** ERROR *** %d error(s) during the cache update." error in
	Bolt.Logger.log "" Bolt.Level.ERROR logmsg
    )

let compute_org verbose cpucore strict prefix depth vlist diffs correl (annots:Ast_org.orgarray) (orgs:Ast_org.orgarray) :
    (int * int) * ((Ast_diff.path * Ast_org.bugs list) list) =
  Debug.profile_code_silent "compute_org"
    (fun () ->
      Bolt.Logger.log "" Bolt.Level.INFO "*** CORRELATION - PHASE 1 ***";
      let (initial_count, ks) = compute_bug_chain verbose strict prefix depth 0 vlist diffs correl orgs in
      let logmsg=Printf.sprintf "*** CORRELATION - PHASE 1 *** %d automatic correlations so far" initial_count in
      Bolt.Logger.log "" Bolt.Level.INFO logmsg;
      (* Update gumtree cache with missing files *)
      let re = Str.regexp_string "&&" in
      let (cmds, ks2) = List.split ks in
      let (dirs, cleaned_cmds) =
	List.fold_left
	  (fun (dir_list, cmd_list) x_opt ->
	    match x_opt with
	      Some x ->
		let (dir, cmd) = match Str.split re x with
		    dir::[cmd] -> (dir, cmd)
		  | _ -> failwith ("Wrong command: x")
		in
		let dirs = if not (List.mem dir dir_list) then
		    dir::dir_list else dir_list
		in
		let cmds = if not (List.mem cmd cmd_list) then
		    cmd::cmd_list else cmd_list
		in (dirs, cmds)
	      | None ->
		(dir_list, cmd_list)
	  )
	  ([],[]) cmds
      in
      if cleaned_cmds <> [] then
	begin
          let logmsg=Printf.sprintf "*** UPDATING GUMTREE CACHE *** %d item(s)." (List.length cleaned_cmds) in
	  Bolt.Logger.log "" Bolt.Level.INFO logmsg;
	  List.iter (fun cmd ->
	    match
	      Unix.system cmd
	    with
		Unix.WEXITED 0 -> ()
	      | Unix.WEXITED 1 -> ()
	      | Unix.WEXITED i -> (let logmsg=Printf.sprintf "*** FAILURE *** Code: %d %s" i cmd in Bolt.Logger.log "" Bolt.Level.ERROR logmsg)
	      | _ -> (let logmsg=Printf.sprintf "*** FAILURE *** %s" cmd in Bolt.Logger.log "" Bolt.Level.ERROR logmsg)
	  ) dirs;
	  pariter cpucore (fun cmd ->
              (let logmsg=Printf.sprintf "Run %s" cmd in
	    Bolt.Logger.log "" Bolt.Level.TRACE logmsg);
	    let status =
	      match
		Unix.system cmd
	      with
		  Unix.WEXITED 0 -> true
		| Unix.WEXITED 1 -> false
		| Unix.WEXITED i -> (let logmsg=Printf.sprintf "*** FAILURE *** Code: %d %s" i cmd in Bolt.Logger.log "" Bolt.Level.ERROR logmsg); false
		| _ -> (let logmsg=Printf.sprintf "*** FAILURE *** %s" cmd in Bolt.Logger.log "" Bolt.Level.ERROR logmsg); false
	    in
	    if status then
	      Unix.execv "/bin/true" (Array.of_list [])
	    else
	      Unix.execv "/bin/false" (Array.of_list [])
	  ) cleaned_cmds;
	end;
      (*	*)
      let logmsg=Printf.sprintf "*** CORRELATION - PHASE 2 *** %d item(s)." (List.length ks2) in
      Bolt.Logger.log "" Bolt.Level.INFO logmsg;
      let count =
	List.fold_left
	  (fun acc (f, bug) ->
	    acc + f bug
	  ) initial_count ks2
      in
      let logmsg=Printf.sprintf "*** CORRELATION - PHASE 2 *** %d automatic correlations" count in
      Bolt.Logger.log "" Bolt.Level.INFO logmsg;
      (*	*)
      let (new_bugs, correlorg) =
	List.split (
	    List.map (fun file ->
                (let logmsg=Printf.sprintf "COMPUTE ORG - Processing file %s" file in
	    Bolt.Logger.log "" Bolt.Level.DEBUG logmsg);
	     (* 		  let sorted = sort_bugs strict prefix bugs in *)
	    let sorted = extract_chain file orgs in
	    let (stats, updbugs) = List.split (List.map (update_status vlist annots) sorted) in
 	    let new_bugs = List.fold_left (+) 0 stats in
	    (new_bugs), (file, updbugs)
	  ) (unique_file_list orgs)
	)
      in
      let new_bug_count = List.fold_left (+) 0 new_bugs in
      ((count, new_bug_count), correlorg)
    )
