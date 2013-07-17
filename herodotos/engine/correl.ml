
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

let may_have_changed strict prefix vlist bfl bug =
  let (_, s, _, file, ver, pos, _, t, _, next, _) = bug in
    next = {Ast_org.def = None}
      && List.exists
      (fun (_, _, _, file2, ver2, pos2, _, t2, head, _, _) ->
	 let vn = Misc.get_next_version vlist ver in
	   file = file2
	     && vn = ver2
	     && head.Ast_org.is_head = true
      ) bfl

let find_disappeared strict prefix vlist (orgs:Ast_org.bug list) =
  List.find_all (may_have_changed strict prefix vlist orgs) orgs

let is_SAME_as_next_in_correl correl file ver pos =
  List.exists
    (fun (cs, _, _, _, cnfile, cnver, cnpos, _) ->
       cs = Ast_org.SAME
	&& cnfile = file
	&& cnver  = ver
	&& cnpos  = pos
    ) correl

let find_all_next strict prefix vlist orgs correl bug =
  let (_, _, _, file, ver, pos, _, t, _, _, _) = bug in
  let (_, cb, ce) = pos in
    (*
      First, we find all bugs in next version with the same
      pattern size.
    *)
    List.find_all
      (fun next2 ->
	 let (_, _, _, file2, ver2, pos2, _, t2, _, _, _) = next2 in
	 let (_, cb2, ce2) = pos2 in
	 let vn = Misc.get_next_version vlist ver in
	   file = file2 && vn = ver2
	     (* Expression have at least the same length
		So, a buggy pattern should not be edited,
		but the line may have been edited.
	     *)
	     && (ce-cb) = (ce2-cb2)
	     && (if strict then
		   let new_t = Org.clean_link_text prefix ver file pos t in
		   let new_tb = Org.clean_link_text prefix ver2 file pos2 t2 in
		     if !Misc.debug then
		       begin
			 prerr_string ("FIND_ALL: \""^ new_t^ "\"");
			 prerr_endline (" <-> \""^ new_tb^ "\"");
		       end;
		     new_t = new_tb
		 else true)
	     && not (is_SAME_as_next_in_correl correl file vn pos2)
	     (*
	       We then remove bugs already in relation with another one.
	     *)
	     && not (List.exists
		       (fun (_, _, _, _, _, _, _, _, _, next, _) ->
			  next = {Ast_org.def = Some next2}
		       )
		       orgs)
      ) orgs

let exists_bug_for_correl bugs cbug =
  List.exists (fun bug ->
	       let (cs, cfile, cver, cpos, _, _, _,_) = cbug in
	       let (_, _, _, file, ver, pos, _, _, _, _, _) = bug in
		 cfile = file
		   && cver = ver
		   && cpos = pos
		   && cs = Ast_org.SAME
	    ) bugs

let get_t_of_bug_for_next_correl strict prefix bugs cbug =
  let (_, _, _, _, _, _, _, t, _, _, _) =
    List.find (fun bug ->
	       let (cs, _, _, _, cfile, cver, cpos, ct) = cbug in
	       let (_, _, _, file, ver, pos, _, t, _, _, _) = bug in
		 cfile = file
		   && cver = ver
		   && cpos = pos
	    ) bugs
  in
    t

let get_all_correl strict prefix correl bugs bug =
  List.find_all
    (fun cbug ->
       let (cs, cfile, cver, cpos, cnfile, cnver, cnpos, t) = cbug in
       let (_, _, _, file, ver, pos, _, bug_t, _, _, _) = bug in

	 cfile = file
	   && cver = ver
	   && cpos = pos
	   && (if strict then
		 let next_t = get_t_of_bug_for_next_correl strict prefix bugs cbug in
		 let new_t = Org.clean_link_text prefix ver file pos t in
		 let new_tb = Org.clean_link_text prefix ver file pos bug_t in
		 let new_nt = Org.clean_link_text prefix cnver file cnpos next_t in
		   if !Misc.debug then
		     begin
		       prerr_string ("get_all_correl: \""^ new_t^ "\"");
		       prerr_endline (" <-> \""^ new_tb^ "\"");
		     end;
		   new_t = new_tb && new_t = new_nt
	       else true)
    ) correl

let gen_todo ch strict vlist prefix orgs correl bug =
  (* TODO : Also propose correlation when basename is identical *)
  (*
    For remaining disappearing bugs, propose correlation with
    every available bug in next.
  *)
  let next = find_all_next strict prefix vlist orgs correl bug in
  List.iter (fun nbug ->
	       Printf.fprintf ch "* TODO %s\n %s\n"
		 (Org.make_orglinkbug true prefix bug)
		 (Org.make_orglinkbug true prefix nbug)
	    )
    next;
    List.length next

let correlate verbose strict prefix vlist correlfile prefix rev_correl orgs =
  (try
     Unix.rename correlfile (correlfile^".bk");
   with _ -> ()
  );
  let ch = open_out correlfile in
  let correl = Misc.unique_list (List.rev rev_correl) in
  let buglist = List.flatten (List.map (fun x -> List.flatten (snd x)) orgs) in
  let disps = find_disappeared strict prefix vlist buglist in
  let todo = List.map (fun bug ->
		 try
		   (*
		     For bugs that still disappear, we keep the UNRELATED info.
		     It should not have SAME correlation here because SAME
		     implies a correlation and thus the bug doesn't disappear
		     anymore ! :)
		     Useless SAME are filtered above.
		   *)
		   let bug_correl = get_all_correl strict prefix correl buglist bug in
		     if bug_correl = [] then
		       (*
			 We don't know anything from previous execution.
			 But we may propose some TODO.
		       *)
		       gen_todo ch strict vlist prefix buglist correl bug
		     else
		       let todo = List.map
			 (fun (s, file, ver, pos, nfile, nver, npos, t) ->
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
		 with _ ->
		   gen_todo ch strict vlist prefix buglist correl bug
	      ) disps in
    List.iter (fun  cbug ->
		 let (s, file, ver, pos, nfile, nver, npos, t) = cbug in
		 let bug = (1, s, "", file, ver, pos, "ovl-face1", t, {Ast_org.is_head=true},{Ast_org.def = None},[]) in
		 let nbug =
		   (max_int, s, "", nfile, nver, npos, "ovl-face1", "", {Ast_org.is_head=false}, {Ast_org.def = None},[]) in
		   if s = Ast_org.SAME then
		     if exists_bug_for_correl buglist cbug then
		     (*
		       We keep SAME correlation information
		       but prune by default others.
		     *)
		     Printf.fprintf ch "* SAME %s\n %s\n"
		       (Org.make_orglinkbug true prefix bug)
		       (Org.make_orglinkbug true prefix nbug)
		     else
		       prerr_endline "*** INFO *** Drop an old SAME entry of a now non-existing bug"
	      ) correl;
    Printf.fprintf ch "\n* org config\n#+SEQ_TODO: TODO | SAME UNRELATED\n";
    close_out ch;
    List.fold_left (+) 0 todo (* Count the number of TODO generated *)
