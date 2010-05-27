open Helper

let age vlist vminopt factor bugs idx =
  let (sum, count) = List.fold_left (fun (sum, count) bug ->
		    let (_, _, _, _, bmin_init, bmax, _) = bug in
		    let bmin = max_idx_w_vminopt vlist vminopt bmin_init in
		      if bmin <= idx && idx <= bmax then
			let (_, dmin, _, _) = Array.get vlist bmin in
			let (_, dmax, _, _) = Array.get vlist bmax in
			  (sum + (dmax - dmin), count +1)
		      else
			(sum, count)
		 ) (0,0) bugs
  in
    if count = 0 then
      None
    else
      Some ((float_of_int sum) /. (factor *. (float_of_int count)))

let avgage vlist bugs grinfo vminopt =
  let (_,_, _, _, _, _, factor) = grinfo in
    wrap_single (Array.init (Array.length vlist) (age vlist vminopt factor bugs))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, ceil (get_ymax1 evol))

let dfts = ("Average age", "Project", "Average age (in years)", 365.25, xmax, ymax, false, avgage)
