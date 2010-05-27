open Helper

let get_expertise vlist idx name =
  if name = "Linus Torvalds" && idx=0 then
    (* Ignore initial import to git done by Linus. *)
    None
  else
    try
      let (contact, since, until, totaltimespan, dates, duration, abspct, reldays) =
	Git.author_info vlist idx name
      in
      let sum = Git.count_patches name vlist idx dates in
	Some (reldays * sum)
    with Not_found ->
      prerr_endline ("Author not found: "^name);
      None

let expertise vlist bugs grinfo vminopt =
  let vmin_idx = match vminopt with
      None -> 0
    | Some vmin -> Misc.get_idx_of_version vlist vmin
  in
  let (_,_, _, _, _, _, factor) = grinfo in
  let (cumulexpertise, count) =
    List.fold_left (fun (sum, count) bug ->
		      let (authoropt,_, _, _, bmin, _, _) = bug in
			if vmin_idx <= bmin then
			  match authoropt with
			      None -> (sum, count)
			    | Some name ->
				let expopt = get_expertise vlist bmin name in
				  match expopt with
				      None     -> (sum, count)
				    | Some exp -> (sum+exp, count+1)
			else
			  (sum, count)
		   )
      (0,0) bugs
  in
    if count = 0 then
      wrap_single (Array.make 1 None)
    else
      let avgexpertise = float_of_int cumulexpertise /. float_of_int count in
	wrap_single_some (Array.make 1 (avgexpertise /. factor))

let xmax _ cumuls = float_of_int (List.length cumuls)
let ymax cumuls = (0.0, ceil (get_ymax1 cumuls))

let dfts = ("Expertise", "Project", "Expertise (weighted patch #)", 100000.0, xmax, ymax, true, expertise)
