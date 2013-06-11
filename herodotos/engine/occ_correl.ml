
let match_bug strict prefix bug1 bug2 =
  let (_, s, _, f, v, pos, _, t, h, _, _) = bug1 in
  let (_, sb, _, fb, vb, posb, _, tb, hb, _, _) = bug2 in
    f = fb && v = vb && pos = posb
(*       && h.Ast_org.is_head = true *)
(*       && hb.Ast_org.is_head = true *)
      && (if strict then
	    let new_t = Org.clean_link_text prefix v f pos t in
	    let new_tb = Org.clean_link_text prefix v f pos tb in
	      if !Misc.debug then
		begin
		  prerr_string ("MATCH_BUG: \""^ new_t^ "\"");
		  prerr_endline (" <-> \""^ new_tb^ "\"");
		end;
	      new_t = new_tb
	  else true)

(*
let rm_bug_chain strict prefix bugs chain =
  List.filter
    (fun bug ->
       not (List.exists (match_bug strict prefix bug) chain)
    ) bugs

let rec get_chain (bugs:Ast_org.bug list) (bug:Ast_org.bug) =
  let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
    match n.Ast_org.def with
	None -> [bug]
      | Some nbug -> bug::get_chain bugs nbug
*)

let rec get_chain (bug:Ast_org.bug) =
  let (l, s, r, f, v, pos, face, t, h, n, sub) = bug in
    match n.Ast_org.def with
	None -> [bug]
      | Some nbug -> bug::get_chain nbug

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

(*
let sort_ver vlist subs =
  List.sort
    (fun (_, _, _, _, ver1, _, _, _, _, _) (_, _, _, _, ver2, _, _, _, _, _) ->
       let vidx1 = Misc.get_idx_of_version vlist ver1 in
       let vidx2 = Misc.get_idx_of_version vlist ver2 in
	 vidx1 - vidx2)
    subs

let rec sort_bugs strict prefix bugs =
  match bugs with
      [] -> []
    | hd::tail ->
	let chain = get_chain bugs hd in
	let ntail = rm_bug_chain strict prefix tail chain in
	  chain :: sort_bugs strict prefix ntail
*)

let get_bug strict prefix (bug: Ast_org.bug) (bugs: Ast_org.bug list) : Ast_org.bug =
  List.find (match_bug strict prefix bug) bugs

let get_next_list vlist bugs f vidx =
  let (_, tbl) = Array.get bugs (vidx+1) in
    try
      Hashtbl.find tbl f
    with Not_found -> []

let update_nohead bug =
  let (l, s, r, f, v, p, face, t, h, n, subs) = bug in
    h.Ast_org.is_head <- false

let check_next verbose strict prefix depth vlist diffs correl (bugs:Ast_org.orgarray) (bug:Ast_org.bug) check_pos =
  Debug.profile_code_silent "check_next"
    (fun () ->
       let (l, s, r, f, v, p, face, t, h, n, _) = bug in
       let t = if strict then (Org.clean_link_text prefix v f p t) else "" in
       let vidx = Misc.get_idx_of_version vlist v in
       let subbugs = get_next_list vlist bugs f vidx in
       let vn = Misc.get_version_name vlist (vidx+1) in
       let check = (l, s, r, f, vn, check_pos, face, t, {Ast_org.is_head=true}, {Ast_org.def=None}, []) in
	 if verbose then
	   (
	     prerr_string "check_next of ";
	     Org.show_bug true bug;
	     prerr_string (" at ");
	     Org.show_bug true check;
	     prerr_newline ();
	     prerr_endline ("List:");
	     List.iter (Org.show_bug true) subbugs;
	     prerr_endline ("\n=========");
	   );
	 try
	   let next = get_bug strict prefix check subbugs in
	     if (n.Ast_org.def = None) then
	       begin
		 n.Ast_org.def <- Some next;
		 update_nohead next;
		 1 (* One automatic correlation performed *)
	       end
	     else
	       0 (* No automatic correlation performed *)
	 with Not_found ->
	   if verbose then prerr_endline ("Trying manual correlation of "^f^" ver. "^v);
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
						    if !Misc.debug then
						      begin
							prerr_string ("\""^ new_t^ "\"");
							prerr_endline (" <-> \""^ t^ "\"");
						      end;
						    new_t = t
						else true
					       )
				     ) correl in
	     let (_, _, _, _, nfile, nver, next_pos, _) = correlb in
	     let check = (l, s, r, nfile, nver, next_pos, "", t, {Ast_org.is_head=true}, {Ast_org.def=None}, []) in
	       (try
		  let next = get_bug strict prefix check subbugs in
		    n.Ast_org.def <- Some next;
		    update_nohead next;
		    if verbose then prerr_endline "Manual correlation OK";
		    0 (* No automatic correlation performed *)
		with Not_found ->
		  if verbose then prerr_endline "Manual correlation KO";
		  0 (* No automatic correlation performed *)
	       )
	   with Not_found ->
	     if verbose then prerr_endline "Manual correlation KO - KO";
	     0 (* No automatic correlation performed *)
    )

let compute_bug_next verbose strict prefix depth vlist diffs correl bugs bug =
  Debug.profile_code_silent "compute_bug_next"
    (fun () ->
       let (l, s, r, f, v, p, face, t, h, n, _) = bug in
	 if verbose then
	   (
	     Org.show_bug true bug;
	     prerr_newline ()
	   );
	 let diffcheck_pos = Diff.compute_new_pos diffs f v p in
	   if verbose then
	     (prerr_string (Org.get_string_pos p);
	      prerr_string " as new pos: ";
	      match diffcheck_pos with
		  (Diff.Sing line,colb,cole) ->
		    let check_pos = (line, colb, cole) in
		      prerr_endline (Org.get_string_pos check_pos)
		| (Diff.Deleted, _,_) ->
		    prerr_endline "Deleted line"
		| (Diff.Cpl (lineb,linee),colb, cole) ->
		    prerr_endline "Somewhere"
	     );
	   match diffcheck_pos with
	       (Diff.Sing line,colb,cole) -> (* We are between two hunks. *)
		 check_next verbose strict prefix depth vlist diffs correl bugs bug (line,colb,cole)
	     | (Diff.Deleted, colb,cole) ->
		 (*
		   We are inside a hunk and lines have been removed.
		   Check for manual correlation.
		 *)
		 check_next verbose strict prefix depth vlist diffs correl bugs bug (0,colb,cole)
	     | (Diff.Cpl (lineb,linee),colb, cole) -> (* We are inside a hunk. *)
		 (*
		   Could we do something for bugs inside a hunk ?
		   It seems to be impossible and worthless.
		   Just check for manual correlation.
		 *)
		 check_next verbose strict prefix depth vlist diffs correl bugs bug (0,colb,cole)
    )

let compute_bug_chain verbose strict prefix depth count vlist diffs correl bugs =
  Debug.profile_code_silent "compute_bug_chain"
    (fun () ->
       let bound = ((Array.length vlist) - 1) in
	 snd (Array.fold_left
		(fun (i, res) (flist, tbl) ->
		   if i < bound then
		     begin
		       (i+1, 
			List.fold_left
			  (fun acc file ->
			     let subbugs = Hashtbl.find tbl file in
			       List.fold_left
				 (fun acc bug ->
				    acc + (compute_bug_next verbose strict prefix depth vlist diffs correl bugs bug)
				 ) acc subbugs
			  ) res flist
		       )
		     end
		   else
		     (i+1, res)
		) (0, 0) bugs
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

let compute_org verbose strict prefix depth vlist diffs correl (annots:Ast_org.orgarray) (orgs:Ast_org.orgarray) :
    (int * int) * ((Ast_diff.path * Ast_org.bugs list) list) =
  Debug.profile_code_silent "compute_org"
    (fun () ->
       let count = compute_bug_chain verbose strict prefix depth 0 vlist diffs correl orgs in
       let (new_bugs, correlorg) =
	 List.split (
	   List.map (fun file ->
		       if !Misc.debug then prerr_endline ("COMPUTE ORG - Processing file " ^file);
			 (* 		  let sorted = sort_bugs strict prefix bugs in *)
		       let sorted = extract_chain file orgs in
		       let (stats, updbugs) = List.split (List.map (update_status vlist annots) sorted) in
 		       let new_bugs = List.fold_left (+) 0 stats in
			 (new_bugs), (file, updbugs))
	     (unique_file_list orgs)
	 )
       in
       let new_bug_count = List.fold_left (+) 0 new_bugs in
	 ((count, new_bug_count), correlorg)
    )
