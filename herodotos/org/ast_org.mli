type orgoption =
    Line of int
  | ColB of int
  | ColE of int
  | Face of string

type pos = int * int * int
type path = string
type vname = string
type text = string
type link = path * orgoption list * text

type status =
    EMPTY
  | TODO
  | OK
  | BUG
  | FP
  | SAME
  | UNRELATED
  | UNKNOWN
  | IGNORED
  | Unknow of string

(* level * status * reason phrase * link * sub-items *)
type org = Org of int * status * text * link * orgs
and orgs = org list

type is_head = {mutable is_head: bool}

(* level * status * reason phrase * file name * version name * position * font color * link text * is_head * next bug * sub-items *)
type bug = int * status * text * path * vname * pos * string * text * is_head * next * orgs
and next = {
    mutable def: bug option
}

type bugs = bug list

type orgarray = (path list * (path, bugs) Hashtbl.t) array
