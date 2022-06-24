%{

%}

%token EOL
%token TSTAR TSLASH TCOLON TEQUAL TSPACE
%token TVERT THASHPLUS TLAB TRAB
%token TVIEW TFACE TLINB TCOLB TCOLE TORG TCONFIG
%token TSTODO TTODO TOK TBUG TNONBUG TFP TSAME TUNRELATED TUNKNOWN TIGNORED
%token<int> TInt
%token<string> TId TTEXT

%start tail
%type <unit> tail

%start oneline
%type <int * Ast_org.org> oneline

%start link
%type <Ast_org.link> link

%%

tail:
   nonempty_list(TSTAR) TSPACE TORG TSPACE TCONFIG EOL      {      }
 | THASHPLUS TSTODO TCOLON TSPACE TTODO TSPACE TVERT nonempty_list(tailelm) EOL        {}

tailelm:
  TSPACE lm {}

lm:
   TBUG       {}
 | TFP        {}
 | TNONBUG    {}
 | TSAME      {}
 | TUNRELATED {}
 | TUNKNOWN   {}
 | TIGNORED   {}
 | TId        {}

oneline:
   level=nonempty_list(TSTAR) nonempty_list(TSPACE) s=status r=reason l=link
    { let lvl = List.length level in
      let new_r = Str.global_replace (Str.regexp " *$") "" r in
	(lvl, Ast_org.Org(lvl, s, new_r, l, []))  }
 | list(TSPACE)     l=link
    { (max_int, Ast_org.Org(max_int, Ast_org.EMPTY, "", l,[])) }

status:
                                       { Ast_org.EMPTY     }
 | s=status_elm  nonempty_list(TSPACE) { s                 }

link:
   TLAB TLAB TVIEW TCOLON p=path ops=list(ol_option) TRAB TLAB t=text TRAB TRAB
                                     { (p, ops, t) }

path:
    p=list(path_elt)                 {     "/" ^ (String.concat "/" p) }
 |  e=TId p=list(path_elt)           { e ^ "/" ^ (String.concat "/" p) }

path_elt:
    TSLASH e = elt {e}

elt:
    e1=TId e2=list(split_path_elt)        { e1 ^ (String.concat "" e2)}  
 |  TEQUAL e=list(split_path_elt)         { "=" ^ (String.concat "" e)} 
 |  TCONFIG                               { "config"        }
 |  e=TInt                                { string_of_int e }
 |  e=TTEXT                               {e               }

split_path_elt:
   e=TId                             {e}
 | TEQUAL                          {"="}

ol_option:
    TCOLON TCOLON v = org_option {v}

org_option:
   TLINB TEQUAL v=TInt   { Ast_org.Line v }
 | TCOLB TEQUAL v=TInt   { Ast_org.ColB v }
 | TCOLE TEQUAL v=TInt   { Ast_org.ColE v }
 | TFACE TEQUAL v=TId    { Ast_org.Face v }

text:
    el=list(textelm)                    { String.concat "" el }

reason:
                                        { ""              }
 |  h=textelm_nospecial t=text          { h ^ t }

textelm:
    t=textelm_nost                      { t               }
 |  s=status_elm                        { Org_helper.get_status s    }

textelm_nost:
    t=textelm_nospecial                 { t               }
 |  TSPACE                              { " "             }

textelm_nospecial:
    id=TId                              { id              }
 |  t=TTEXT                             { t               }
 |  i=TInt                              { string_of_int i }
 |  TSLASH                              { "/"             }
 |  TCOLON                              { ":"             }
 |  TCONFIG                             { "config"        }
 |  TSTAR                               { "*"             }
 |  TEQUAL                              { "="             }
 |  THASHPLUS                           { "#+"            }
 |  TIGNORED                            { "IGNORED"       }
 |  TVIEW                               { "view"          }
(* |  s=status_elm                        { Org_helper.get_status s    } *)

status_elm:
   TTODO                             { Ast_org.TODO      }
 | TOK                               { Ast_org.OK        }
 | TUNKNOWN                          { Ast_org.UNKNOWN   }
(*  | TIGNORED                          { Ast_org.IGNORED   } *)
 | TBUG                              { Ast_org.BUG       }
 | TFP                               { Ast_org.FP        }
 | TSAME                             { Ast_org.SAME      }
 | TUNRELATED                        { Ast_org.UNRELATED }
(* | s=TId                             { Ast_org.Unknow(s) } *)
