open Helper

let neg_opt optvalue =
  match optvalue with
      None   -> None
    | Some v -> Some (-. v)

let evolution vlist bugs grinfo vminopt =
  let size = Array.length vlist in
  let births = Array.init (Array.length vlist) (Birth.count_new bugs) in
  let deaths = Array.init size (Death.count_death bugs size) in
  let neg_deaths = Array.map neg_opt deaths in
    combine births neg_deaths

let xmax vers _ = ceil (get_xmax vers)
let ymax evol =
  let values = get_ymax2 evol in
    (floor (snd values) -. 1.0, ceil (fst values) +. 1.0)

let dfts = ("Net increase EVOLUTION", "Project", "Net increase of defects", 1000.0, xmax, ymax, false, evolution)
