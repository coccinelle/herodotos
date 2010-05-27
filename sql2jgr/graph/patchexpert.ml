open Helper

let get_expertise vlist idx name =
  if name = "Linus Torvalds" && idx=0 then
    (* Ignore initial import to git done by Linus. *)
    None
  else
    try
      let (contact, since, until, totaltimespan, dates, duration, abspct, relpct) =
	Git.author_info vlist idx name
      in
      let sum = Git.count_patches name vlist idx dates in
	Some sum
    with Not_found ->
      prerr_endline ("Author not found: "^name);
      None

let expertise vlist bugs grinfo vminopt =
  let (cumulexpertise, count) =
    List.fold_left (fun (sum, count) bug ->
		      let (authoropt,_, _, _, bmin, _, _) = bug in
			match authoropt with
			    None -> (sum, count)
			  | Some name ->
			      let expopt = get_expertise vlist bmin name in
				match expopt with
				    None     -> (sum, count)
				  | Some exp -> (sum+exp, count+1)
		   )
      (0,0) bugs
  in
    if count = 0 then
      wrap_single_some (Array.make 1 0.0)
    else
      let avgexpertise = float_of_int cumulexpertise /. float_of_int count in
	wrap_single_some (Array.make 1 avgexpertise)

let xmax _ cumuls = float_of_int (List.length cumuls)
let ymax cumuls = (0.0, ceil (get_ymax1 cumuls))

let dfts = ("Patch expertise", "Project", "Patch expertise (in #)", 1.0, xmax, ymax, true, expertise)
