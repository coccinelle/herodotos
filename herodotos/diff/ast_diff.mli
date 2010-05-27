
type pos = int * int
type hunk = pos * pos
type path = string
type vname = string

type diff = (vname * path) * hunk list
type diffs = diff list
