open Helper

let count_in bugs idx =
  List.fold_left (fun counter bug ->
		    let (_,_, _, _, bmin, bmax, _) = bug in
		      if idx >= bmin && idx <= bmax then
			counter +. 1.0
		      else
			counter
		 ) 0.0 bugs

let evolution vlist bugs _ vminopt =
  wrap_single_some (Array.init (Array.length vlist) (count_in bugs))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, get_ymax1 evol)

let dfts = ("USER-DEFINED EVOLUTION", "", "", 1.0, xmax, ymax, false, evolution)
