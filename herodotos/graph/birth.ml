open Helper

let count_new bugs idx =
  if idx = 0 then
    None
  else
    Some (
      List.fold_left (fun counter bug ->
			let (_, _, _, _, bmin, _, _) = bug in
			  if idx = bmin then  (* && idx != 0 *)
			    counter +. 1.0
			  else
			    counter
		     ) 0.0 bugs
    )

let evolution vlist bugs _ vminopt =
  wrap_single (Array.init (Array.length vlist) (count_new bugs))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, ceil (get_ymax1 evol) +. 1.0)

let dfts = ("BIRTH EVOLUTION", "Project", "New Defects", 1000.0, xmax, ymax, false, evolution)
