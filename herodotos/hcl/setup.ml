
exception BadConfiguration of string

let prefix = ref ""
let smatchdir = ref ""
let projectsdir = ref ""
let resultsdir = ref ""
let websitedir = ref ""
let findcmd = ref ""
let spflags = ref ""
let cpucore = ref None
let findchild = ref None
let dbconn = ref ""

type project = string
type defect = string
type graph = string
type experience = string

module Prj =
struct
  type t = project
  let equal = (=)
  let hash = Hashtbl.hash
end

module Dft =
struct
  type t = defect
  let equal = (=)
  let hash = Hashtbl.hash
end

module Gph =
struct
  type t = graph
  let equal = (=)
  let hash = Hashtbl.hash
end

module Exp=
struct
  type t = experience
  let equal = (=)
  let hash = Hashtbl.hash
end 

module PrjTbl = Hashtbl.Make (Prj)

module DftTbl = Hashtbl.Make (Dft)

module GphTbl = Hashtbl.Make (Gph)

module ExpTbl = Hashtbl.Make (Exp)

let projects = PrjTbl.create 5
let smatchs = DftTbl.create 11
let graphs = GphTbl.create 97
let experiments = ExpTbl.create 97

let setRef p r msg =
  if (String.length !r) = 0 then
    r := p
  else
    raise (BadConfiguration msg)

(*
let setRefOwt p r msg =
  if (String.length !r) = 0 then
    r := p
  else
    (
      prerr_endline (msg^": overwriting");
      r := p
    )
*)

let setPrefix p = setRef p prefix "prefix is already set"
let setSmatchDir p = setRef p smatchdir "semantic match directory is already set"
let setPrjDir p = setRef p projectsdir "projects directory is already set"
let setResultsDir p = setRef p resultsdir "results directory is already set"
let setWebsiteDir p = setRef p websitedir "website directory is already set"
let setDBConn db = setRef db dbconn "Database connexion information is already set"

let setFindCmd c = setRef c findcmd "pattern matching tool is already set"
let setFindChild (c: int) = findchild := Some c

let setSPFlags v = setRef v spflags "spatch flags are already set"
let setCPUcore (c: int) = cpucore := Some c
let getCPUcore () =
  match !cpucore with
      None -> Misc.get_number_of_cores ()
    | Some cpucore -> cpucore

(* For projects *)
let addPrj (name:string) (prj : (int * (string * int * Unix.tm option * int) array) option * Ast_config.attr list) =
  if PrjTbl.mem projects name then
    raise (BadConfiguration ("project "^name^" is already declared."))
  else
    PrjTbl.add projects name prj

let updPrj (name:string) (prj : (int * (string * int * Unix.tm option * int) array) option * Ast_config.attr list) =
  PrjTbl.replace projects name prj

(* For patterns *)
let addDft (name: string) (dft: Ast_config.attr list) =
  if DftTbl.mem smatchs name then
    raise (BadConfiguration ("pattern "^name^" is already declared."))
  else
    DftTbl.add smatchs name dft

let updDft (name: string) (dft: Ast_config.attr list) =
  DftTbl.replace smatchs name dft

(* For graphs *)
let addGph (name: string) (gph: Ast_config.gph) =
  if GphTbl.mem graphs name then
    raise (BadConfiguration ("graph "^name^" is already declared."))
  else
    GphTbl.add graphs name gph

let updGph (name: string) (gph: Ast_config.gph) =
    GphTbl.replace graphs name gph

(* for experiments*)
let addExp (name: string) (exp: Ast_config.experiment) =
  if ExpTbl.mem experiments name then
    raise (BadConfiguration ("experiment "^name^" is already declared."))
  else
    ExpTbl.add experiments name exp

let updExp (name: string) (exp: Ast_config.experiment) =
    ExpTbl.replace experiments name exp

