open Helper

let get_expertise vlist idx name =
  if name = "Linus Torvalds" && idx=0 then
    (* Ignore initial import to git done by Linus. *)
    None
  else
    try
      let (contact, since, until, totaltimespan, sum, duration, abspct, relpct) =
	Git.author_info vlist idx name
      in
	Some (abspct)
    with Not_found ->
      prerr_endline ("\nAuthor not found: "^name);
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
  let avgexpertise = float_of_int cumulexpertise /. float_of_int count in
    wrap_single_some (Array.make 1 avgexpertise)

let xmax _ cumuls = float_of_int (List.length cumuls)
let ymax cumuls = (0.0, 100.0)

let dfts = ("Abs. time expertise", "Project", "Absolute time expertise (in %)", 1.0, xmax, ymax, true, expertise)
