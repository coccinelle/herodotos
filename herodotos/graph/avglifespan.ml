open Helper

let avg vlist bugs grinfo vminopt =
  let (_,_, _, _, _, _, factor) = grinfo in
  let (_, avgday, _) = sl_ratio_day vlist bugs in
    wrap_single_some (Array.make 1 ((float_of_int avgday) /. factor))

let xmax _ cumuls = float_of_int (List.length cumuls)
let ymax cumuls = (0.0, ceil (get_ymax1 cumuls))

let dfts = ("AVG lifespan", "Project", "Lifespan", 365.25, xmax, ymax, false, avg)
