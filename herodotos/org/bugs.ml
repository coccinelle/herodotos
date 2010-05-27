open Lexing

let filter_bugs ast =
  List.fold_left (fun l b ->
		    let (_, s, _, file, c, vs, p ) = b in
		      match s with
			  Ast_bugs.BUG -> (file, c, vs, p)::l
			| _ ->  l
		 ) [] ast

let minidx vlist v1 v2 =
  let iv1 = Misc.get_idx_of_version vlist v1 in
  let iv2 = Misc.get_idx_of_version vlist v2 in
  let v = min iv1 iv2 in
    Misc.get_version_name vlist v

let maxidx vlist v1 v2 =
  let iv1 = Misc.get_idx_of_version vlist v1 in
  let iv2 = Misc.get_idx_of_version vlist v2 in
  let v = max iv1 iv2 in
    Misc.get_version_name vlist v

(**

*)
let get_buggyfiles bugs =
  let files = List.map (fun (f, _,_,_,_) -> f) bugs in
    Misc.unique_list files

let filter_versions vlist vs =
  let knowns = Array.to_list (Array.map (fun (n, _,_,_) -> n)vlist) in
    List.fold_left
      (fun acc v -> if List.mem v knowns then v::acc else acc) [] vs

let compute_bug vlist (file, c, vs, pos) =
  let vs = filter_versions vlist vs in
    if vs = [] then
      (file, c, -1, -1, pos)
    else
      let idxmax = Array.length vlist - 1 in
      let vidxmax = Misc.get_version_name vlist idxmax in
      let vidxmin = Misc.get_version_name vlist 0 in
      let min_b = List.fold_left (minidx vlist) vidxmax vs in
      let max_b = List.fold_left (maxidx vlist) vidxmin vs in
      let vmin = Misc.get_idx_of_version vlist min_b in
      let vmax = Misc.get_idx_of_version vlist max_b in
	(file, c, vmin, vmax, pos)

let _sort bug1 bug2 =
  let (_, _, vmin1, vmax1, _) = bug1 in
  let (_, _, vmin2, vmax2, _) = bug2 in
    if vmin1 > vmin2 then
      1
    else if vmin1 < vmin2 then
      -1
    else
      if vmax1 > vmax2 then
	-1
      else if vmax1 < vmax2 then
	1
      else
	0

let sort bugs = List.sort _sort bugs

let show_bug vlist bug =
  let (fb, c, min_b, max_b, _) = compute_bug vlist bug in
  let vmin = Misc.get_version_name vlist min_b in
  let vmax = Misc.get_version_name vlist max_b in
    prerr_endline ("Bugs in " ^ fb ^ " from " ^  vmin ^ " to " ^ vmax)

(*
  To compare the results of the correlation with
  the previous results (possibly annotated), if any
*)
let is_equal_bug (org:Ast_org.bug) (annot:Ast_org.bug) =
  org = annot

let is_in_set (annots:Ast_org.bugs) (org:Ast_org.bug) =
  List.exists (is_equal_bug org) annots

(*
let wrap_bugs prefix (bugs:Ast_org.bugs) : Ast_org.bug =
  let bug = List.hd bugs in
  let sub = List.map
      (fun  bug ->
	 let (l, s, r, f, v, pos, face, t, n, sub) = bug in
	 let link = Org.make_orglink_of_bug prefix bug in
	   (* FIXME: wrap_bugs
	      We currently discard sub-elements
	      and erase the status.
	   *)
	   Ast_org.Org(l+1, Ast_org.EMPTY, r, link, [])
      )
      bugs
  in
  let (l, s, r, f, v, pos, face, t, n, deepsub) = bug in
    (l, s, r, f, v, pos, face, t, n, deepsub@sub)
*)
