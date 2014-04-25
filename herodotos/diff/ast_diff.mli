(* For GNU diff *)
type pos = int * int
type hunk = pos * pos

(* For Gumtree *)
(* pos * length * begin line * begin col * end line * end col *)
type position = int * int * int * int * int * int
type tree =
    (* Position before * position after * children nodes *)
    Tree of position * position option * tree list

(* Generic types *)
type changes =
    GNUDiff of hunk list
  | Gumtree of tree
  | DeletedFile

type path = string
type vname = string

type diff = (vname * path) * changes
type diffs = diff list

type lineprediction =
    Deleted
  | Sing of int
  | Cpl of int * int
