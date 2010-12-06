type version = string
type state =
    True
  | False
  | Size of int
  | Empty
type entry = string * state list
type matrix = version list * entry list
