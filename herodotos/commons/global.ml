(* Use "-rcX" for release candidates *)
let version="0.7.0-rc1"

let cocciext  = ".cocci"
let origext   = ".orig.org"
let correlext = ".correl.org"
let patchext  = ".patchset"
let gumtreeext  = ".gumtree"
let editext   = ".edit.org"
let bugext    = ".new.org"
let existext  = ".exist"
let listext   = ".list"
let failed = ".failed"

let sep = "_"

let hacks = ref false

type hmode =
  Help
| Longhelp
| Version

| PreInit
| Init
| Correl
| Graph
| Stat
| Statcorrel
| StatFP
| Test
| Erase
| Blame
| ExpHistory
