type version = string
type state =
    True
  | False
  | Size of int
type entry = string * state list
type matrix = version list * entry list
