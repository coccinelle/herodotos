
let get_status s =
  match s with
      Ast_org.EMPTY     -> ""
    | Ast_org.TODO	-> "TODO"
    | Ast_org.OK	-> "OK"
    | Ast_org.BUG	-> "BUG"
    | Ast_org.FP	-> "FP"
    | Ast_org.SAME	-> "SAME"
    | Ast_org.UNRELATED	-> "UNRELATED"
    | Ast_org.UNKNOWN   -> "UNKNOWN"
    | Ast_org.IGNORED   -> "IGNORED"
    | Ast_org.Unknow st -> st
