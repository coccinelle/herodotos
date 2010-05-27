open Helper

let get_size vlist factor initsize i =
  let (_,_,_,size) = Array.get vlist i in
    (factor *. float_of_int (size)) /. initsize -. factor

let evolution vlist _ grinfo vminopt =
  let (_,_,_,initsize) = Array.get vlist 0 in
  let (_,_, _, _, _, _, factor) = grinfo in
    wrap_single_some (Array.init (Array.length vlist) (get_size vlist factor (float_of_int initsize)))

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, get_ymax1 evol)

let dfts = ("PCT SIZE EVOLUTION", "Projects", "Percentage increase", 100.0, xmax, ymax, false, evolution)
