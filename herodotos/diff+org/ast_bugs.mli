type status =
    TODO
  | OK
  | BUG
  | FP
  | Other of string

type version = string

type sbug = status * string * string * version list * Ast.pos
type sbugs = sbug list

type bug = string * string * version list * Ast_org.pos
type bugs = bug list
