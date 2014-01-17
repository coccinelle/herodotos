
(* For GNU diff *)
type pos = int * int
type hunk = pos * pos

(* For Gumtree *)
(* begin line * begin col * end line * end col *)
type position = int * int * int * int
type action =
    Insert of position
  | Move of position * position (* before * after *)
  | Delete of position

(* Generic types *)
type changes =
    GNUDiff of hunk list
  | Gumtree of action list 

type path = string
type vname = string

type diff = (vname * path) * changes
type diffs = diff list

type lineprediction =
    Deleted
  | Sing of int
  | Cpl of int * int
