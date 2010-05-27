open Helper

let avglifeexpect vlist factor bugs idx =
  let size = Array.length vlist -1 in
  let (wsum, death) =
    List.fold_left (fun (weigthedsum, death) bug ->
		      let (_,_, _, _, bmin, bmax, _) = bug in
			if idx = bmax
			  && bmin <> 0
			  && bmax <> size then
			    let (_, dmin, _, _) = Array.get vlist bmin in
			    let (_, dmax, _, _) = Array.get vlist (bmax+1) in
			    let myage = (dmax - dmin) in
			      (* This bug died at this version *)
			      (weigthedsum + myage, death +1)
			else
			  (weigthedsum, death)
		   ) (0,0) bugs
  in
    if death = 0 then
      None
    else
      Some (float_of_int wsum /. (factor *. (float_of_int death)))

let lifeexpect vlist bugs grinfo vminopt =
  let (_,_, _, _, _, _, factor) = grinfo in
    wrap_single (Array.init (Array.length vlist) (avglifeexpect vlist factor bugs))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, ceil (get_ymax1 evol))

let dfts = ("Life Expectancy", "Project", "Life Expectancy", 365.25, xmax, ymax, false, lifeexpect)
