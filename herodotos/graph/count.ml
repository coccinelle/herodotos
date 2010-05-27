open Helper

let cumul _ bugs _ vminopt =
  wrap_single_some (Array.make 1 (float_of_int (List.length bugs)))

let xmax _ cumuls = float_of_int (List.length cumuls)
let ymax cumuls = (0.0, ceil (get_ymax1 cumuls) +. 1.0)

let dfts = ("ABS COUNT", "Project", "Defect Sum", 1000.0, xmax, ymax, false, cumul)
