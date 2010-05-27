open Helper

let xmax vers _ = ceil (get_xmax vers)
let ymax evol = (0.0, get_ymax1 evol)

let dfts = ("USER-DEFINED EVOLUTION", "", "", 1.0, xmax, ymax, false, Countevol.evolution)
