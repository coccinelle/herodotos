open Helper

let count_death bugs size idx =
  if idx = (size - 1) then
    None
  else
    Some (
      List.fold_left (fun counter bug ->
			let (_,_, _, _, _, bmax, _) = bug in
			  if idx = bmax then (*  && idx < (size-1) *)
			    counter +. 1.0
			  else
			    counter
		     ) 0.0 bugs
    )

let evolution vlist bugs _ vminopt =
  let size = Array.length vlist in
    wrap_single (Array.init size (count_death bugs size))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, ceil (get_ymax1 evol) +. 1.0)

let dfts = ("DEATH EVOLUTION", "Project", "Death Defects", 1000.0, xmax, ymax, false, evolution)
