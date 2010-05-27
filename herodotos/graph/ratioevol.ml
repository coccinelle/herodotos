open Helper

let ratioevol vlist bugs grinfo vminopt =
  let (_,_, _, _, _, _, factor) = grinfo in
  let sumevol = Countevol.evolution vlist bugs grinfo vminopt in
    Array.mapi (fun i wrap_sum ->
		  match wrap_sum with
		      Single (Some sum) ->
			let (_,_,_, size) = Array.get vlist i in
			  Helper.Single (Some ((sum *. factor) /. (float_of_int size)))
		    | _ -> failwith "Unexpected datatype"
	       )
      sumevol

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, get_ymax1 evol)

let dfts = ("Density EVOLUTION", "Project", "Defect density (by KLOC)", 1000.0, xmax, ymax, false, ratioevol)
