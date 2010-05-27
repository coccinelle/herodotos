open Helper

let get_size vlist factor i =
  let (_,_,_,size) = Array.get vlist i in
    if size <> 0 then
      Some (float_of_int (size) /. factor)
    else
      None (* To not draw info. when the size is not available/empty. *)

let evolution vlist _ grinfo vminopt =
  let (_,_, _, _, _, _, factor) = grinfo in
    wrap_single (Array.init (Array.length vlist) (get_size vlist factor))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, get_ymax1 evol)

let dfts = ("SIZE EVOLUTION", "Projects", "Code size", 1.0, xmax, ymax, false, evolution)
