type version = string * Unix.tm * int

type correl = Strict | Nocorrel | Default
type format = CSV | Org

type expression =
    Pattern of string
  | Project of string
  | Cst     of float
  | Plus  of expression * expression
  | Minus of expression * expression
  | Mul   of expression * expression
  | Div   of expression * expression

type attr =
    Author of bool
  | CleanColor of float * float * float
  | Color of float * float * float
  | Correl of string
  | Data of expression
  | SQLData of string
  | DftPattern of string
  | DftProject of string
  | Dir of string
  | Factor of float
  | File of string option
  | Filename of bool
  | Flags of string
  | Footer of string
  | Format of string
  | Info of bool
  | Legend of string
  | LineType of string
  | MarkSize of float
  | MarkType of string
  | NotExistColor of float * float * float
  | PatternColor of float * float * float
  | Prune of bool
  | Ratio of bool
  | SCM of string
  | Size of float * float
  | SubDir of string
  | VMin of string
  | Version of int * version list
  | XAxis of string
  | XLabel of string
  | YLabel of string
  | XLegend of string
  | XMin of float
  | YMin of float
  | YMax of float
  | YAxis of string
  | YLegend of string
  | YLegendFactor of string

type curve = string option * string option * attr list * Ast.pos

type group =
    GrpCurve of string * curve list
  | GrpPatt of string * Ast.pos

type subgraph =
    Curves of curve list
  | Groups of group list

type gph = attr list * subgraph
