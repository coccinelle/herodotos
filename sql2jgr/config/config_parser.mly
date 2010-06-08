%{

  exception BadConfigFormat

    (*
      In jgraph, you could use a '\' follow by a newline '\n'
      to have multiple lines legend. For convenience, we use
      '\n' and convert them to '\' followed by '\n'.
    *)
  let to_jgraph_fmt s =
    let newlinere = Str.regexp_string "\\n" in
      Str.global_replace newlinere "\\\n" s

%}

// TCOLEQ TLAB TRAB

%token EOF
%token TEQUAL TCOMMA TSTAR TSLASH TPLUS TMINUS
%token TLCB TRCB TLPAR TRPAR TSC TCOLON
%token TPROJECT TPATTERN TGRAPH TCURVE TEMPTY TNONE
%token TPREFIX TPROJECTS TWEBSITE TFLAGS TCPUCORE TSQL TDBCONN
%token TSCM TDATA TDIR TSUBDIR TLINESTYLE TMARKTYPE TMARKSIZE TVERSIONS TCORREL TFORMAT
%token TLEGEND TXLEGEND TXMIN TXAXIS TYAXIS TYLEGEND TYLEGENDFACTOR TFACTOR
%token TCOLOR TNOTEXISTCOLOR TCLEANCOLOR TPATTERNCOLOR
%token TFILE TFILENAME TRATIO TGROUP TINFO TSIZE TVMIN TPRUNE TAUTHOR
%token<int> TInt
%token<float> TFloat
%token<string> TId
%token<string> TSTRING2Q
%token<string> TSTRING1Q
%token<bool> TBOOL

%left TPLUS TMINUS
%left TSTAR TSLASH

%start main
%type <unit> main

%%

main:
 list(toplevel) EOF { }

toplevel:
  gbattr  {}
| project {}
| pattern {}
| graph   {}

project:
  TPROJECT name=TId TLCB atts=list(attr) TRCB { Setup.addPrj name (None, atts) }

gbattr:
  TPREFIX      TEQUAL p=TSTRING2Q        { Setup.setPrefix(p)}
| TPROJECTS    TEQUAL p=TSTRING2Q        { Setup.setPrjDir(p)}
| TWEBSITE     TEQUAL p=TSTRING2Q        { Setup.setWebsiteDir(p)}
| TFLAGS       TEQUAL f=TSTRING2Q        { Setup.setFlags(f)}
| TDBCONN      TEQUAL s=TSTRING2Q        { Setup.setDBConn(s)}
| TCPUCORE     TEQUAL c=TInt           { Setup.setCPUcore(c)}

attr:
  TCOLOR         TEQUAL r=float v=float b=float    { Ast_config.Color(r,v,b) }
| TCORREL        TEQUAL dft=TId                    { Ast_config.Correl(dft) }
| TCLEANCOLOR    TEQUAL r=float v=float b=float    { Ast_config.CleanColor(r,v,b)}
| TPATTERN       TEQUAL dft=TId                    { Ast_config.DftPattern(dft)}
| TPATTERNCOLOR  TEQUAL r=float v=float b=float    { Ast_config.PatternColor(r,v,b)}
| TDATA          TEQUAL e=expression               { Ast_config.Data(e)}
| TSQL           TEQUAL e=sql                      { Ast_config.SQLData(e)}
| TDIR           TEQUAL d=path                     { Ast_config.Dir(d)}
| TSUBDIR        TEQUAL d=path                     { Ast_config.SubDir(d)}
| TFACTOR        TEQUAL f=float                    { Ast_config.Factor(f)}
| TFILE          TEQUAL f=TSTRING2Q
    { if f = "" then Ast_config.File(None) else Ast_config.File(Some f) }
| TFILE          TEQUAL TNONE                      { Ast_config.File(None)}
| TFILENAME      TEQUAL b=TBOOL                    { Ast_config.Filename(b)}
| TFORMAT        TEQUAL dft=TId                    { Ast_config.Format(dft) }
| TINFO          TEQUAL b=TBOOL                    { Ast_config.Info(b)}
| TLEGEND        TEQUAL l=TSTRING2Q                  { Ast_config.Legend(l)}
| TXLEGEND       TEQUAL l=TSTRING2Q                  { Ast_config.XLegend(to_jgraph_fmt l)}
| TYLEGEND       TEQUAL l=TSTRING2Q                  { Ast_config.YLegend(to_jgraph_fmt l)}
| TYLEGENDFACTOR TEQUAL f=TId                      { Ast_config.YLegendFactor(f)}
| TLINESTYLE     TEQUAL s=TId                      { Ast_config.LineType(s)}
| TMARKTYPE      TEQUAL m=TId                      { Ast_config.MarkType(m)}
| TMARKSIZE      TEQUAL v=float                    { Ast_config.MarkSize(v)}
| TXAXIS         TEQUAL t=TId                      { Ast_config.XAxis(t)}
| TXMIN          TEQUAL v=float                    { Ast_config.XMin(v)}
| TYAXIS         TEQUAL t=gid                      { Ast_config.YAxis(t)}
| TNOTEXISTCOLOR TEQUAL r=float v=float b=float    { Ast_config.NotExistColor(r,v,b)}
| TPROJECT       TEQUAL prj=TId                    { Ast_config.DftProject(prj)}
| TAUTHOR        TEQUAL b=TBOOL                    { Ast_config.Author(b)}
| TPRUNE         TEQUAL b=TBOOL                    { Ast_config.Prune(b)}
| TRATIO         TEQUAL b=TBOOL                    { Ast_config.Ratio(b)}
| TVERSIONS      TEQUAL TLCB vs=list(version) TRCB {
    let (cl, vl) = List.split vs in
    let count = List.fold_left max 0 cl in
      Ast_config.Version(count, vl)
  }
| TVMIN          TEQUAL v=TSTRING2Q                  { Ast_config.VMin(v)}
| TSCM           TEQUAL v=TSTRING2Q                  { Ast_config.SCM(v)}
| TFLAGS         TEQUAL f=TSTRING2Q                  { Ast_config.Flags(f)}
| TSIZE          TEQUAL x=float y=float            { Ast_config.Size(x,y)}

attrs:
  TLCB atts=list(attr) TRCB {atts}

version:
  TLPAR name=TSTRING2Q TCOMMA d=date TCOMMA size=TInt TRPAR {
    let count = List.length (Str.split (Str.regexp_string (Str.quote "/")) name) in
      (count, (name, d, size))
  }

date:
  m=TInt TSLASH d=TInt TSLASH y=TInt
    { snd (Unix.mktime {Unix.tm_mon=m-1; Unix.tm_mday=d; Unix.tm_year=y-1900;
       (* Don't care about the time *)
       Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
       (* Will be normalized by mktime *)
       Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
      })
    }

pattern:
  TPATTERN name=TId atts=attrs { Setup.addDft name atts }
| TPATTERN name=TId TSC        { Setup.addDft name []   }

graph:
  TGRAPH name=path TLCB atts=list(attr) cs=nonempty_list(curve) TRCB  { Setup.addGph name (atts,Ast_config.Curves(cs)) }
| TGRAPH name=path TLCB atts=list(attr) gs=list(group) TRCB  { Setup.addGph name (atts,Ast_config.Groups(gs)) }

curve:
  TCURVE TPROJECT p=TId            { (Some p,None  ,   [], Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId atts=attrs { (Some p,None  , atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPATTERN d=TId            { (None  ,Some d,   [], Misc.getpos $startpos($1) $endofs) }
| TCURVE TPATTERN d=TId atts=attrs { (None  ,Some d, atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId TPATTERN d=TId atts=attrs
    { (Some p, Some d, atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId TPATTERN d=TId
    { (Some p, Some d, [], Misc.getpos $startpos($1) $endofs) }
| TCURVE name=TSTRING2Q atts=attrs
    {(None, None, Ast_config.Legend(name)::atts,Misc.getpos $startpos($1) $endofs)}
| TEMPTY TCURVE
    { (None, None, [], Misc.getpos $startpos($1) $endofs) }

group:
  TGROUP name=TSTRING2Q TLCB cs=list(curve) TRCB { Ast_config.GrpCurve(name,cs)}
| TGROUP TPATTERN d=TId                          { Ast_config.GrpPatt(d, Misc.getpos $startpos($1) $endofs)}

path:
  p=separated_nonempty_list(TSLASH, gid)        { String.concat "/" p }
| TSLASH p=separated_nonempty_list(TSLASH, gid) { "/"^ String.concat "/" p }

float:
         f=TFloat { f                 }
|        i=TInt   { float_of_int i    }
| TMINUS f=TFloat { -. f              }
| TMINUS i=TInt   { -. float_of_int i }

gid:
  i=TId                    {        i        }
| TCLEANCOLOR              { "nooccurcolor"  }
| TCOLOR                   { "color"         }
| TCORREL                  { "correl"        }
| TCPUCORE                 { "cpucore"       }
| TCURVE                   { "curve"         }
| TDATA                    { "data"          }
| TDIR                     { "dir"           }
| TFACTOR                  { "factor"        }
| TFILE                    { "file"          }
| TFILENAME                { "filename"      }
| TGRAPH                   { "graph"         }
| TGROUP                   { "group"         }
| TINFO                    { "info"          }
| TLEGEND                  { "legend"        }
| TLINESTYLE               { "linestyle"     }
| TMARKTYPE                { "marktype"      }
| TNONE                    { "none"          }
| TNOTEXISTCOLOR           { "notexistcolor" }
| TPATTERN                 { "pattern"       }
| TFORMAT                  { "format"        }
| TPATTERNCOLOR            { "occurcolor"    }
| TPREFIX                  { "prefix"        }
| TPROJECT                 { "project"       }
| TPROJECTS                { "projects"      }
| TRATIO                   { "ratio"         }
| TSIZE                    { "size"          }
| TFLAGS                   { "flags"         }
| TVERSIONS                { "versions"      }
| TWEBSITE                 { "website"       }
| TXAXIS                   { "xaxis"         }
| TXLEGEND                 { "xlegend"       }
| TYAXIS                   { "yaxis"         }
| TYLEGEND                 { "ylegend"       }
| TYLEGENDFACTOR           { "ylegendfactor" }
| b=TBOOL                  { if b then "true" else "false" }

expression:
  TPATTERN d=TId                     { Ast_config.Pattern d     }
| TPROJECT p=TId                     { Ast_config.Project p     }
| f=float                            { Ast_config.Cst f         }
| TLPAR e=expression TRPAR           { e                        }
| e1=expression TPLUS  e2=expression { Ast_config.Plus(e1, e2)  }
| e1=expression TMINUS e2=expression { Ast_config.Minus(e1, e2) }
| e1=expression TSTAR  e2=expression { Ast_config.Mul(e1, e2)   }
| e1=expression TSLASH e2=expression { Ast_config.Div(e1, e2)   }

sql:
 sql=list(sql_tok) TSC               { (String.concat " " sql) ^ ";"}

sql_tok:
    e=gid                         { e               }
  | c=TInt                        { string_of_int c }
  | s=TSTRING2Q                   { "\""^s^"\""     }
  | s=TSTRING1Q                   { "'"^s^"'"       }
  | TMINUS                        { "-" }
  | TPLUS                         { "+" }
  | TSTAR                         { "*" }
  | TSLASH                        { "/" }
  | TCOMMA                        { "," }
  | TEQUAL                        { "=" }
  | TLPAR                         { "(" }
  | TRPAR                         { ")" }
  | TCOLON                        { ":" }
