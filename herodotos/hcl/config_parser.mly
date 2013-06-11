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
%token TLCB TRCB TLPAR TRPAR
%token TPROJECT TPATTERN TGRAPH TCURVE TEMPTY TNONE TEXPERIENCE
%token TPREFIX TCOCCI TPROJECTS TRESULTS TWEBSITE TFINDCMD TSPFLAGS TCPUCORE
%token TFINDCHILD
%token TSCM TDATA TDIR TSUBDIR TLINESTYLE TMARKTYPE TMARKSIZE TVERSIONS TCORREL TFORMAT
%token TLEGEND TXLEGEND TXMIN TXAXIS TYAXIS TYLEGEND TYLEGENDFACTOR TFACTOR TON TWITH
%token TCOLOR TNOTEXISTCOLOR TCLEANCOLOR TPATTERNCOLOR TFOOTER
%token TFILE TFILENAME TRATIO TSORT TGROUP TINFO TSIZE TVMIN TPRUNE TAUTHOR
%token<int> TInt
%token<float> TFloat
%token<string> TId
%token<string> TSTRING
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
| experience {}
| graph   {}

project:
  TPROJECT name=TId TLCB atts=list(attr) TRCB { Setup.addPrj name (None, atts) }

gbattr:
  TPREFIX      TEQUAL p=TSTRING        { Setup.setPrefix(p)}
| TCOCCI       TEQUAL p=TSTRING        { Setup.setSmatchDir(p)}
| TPROJECTS    TEQUAL p=TSTRING        { Setup.setPrjDir(p)}
| TRESULTS     TEQUAL p=TSTRING        { Setup.setResultsDir(p)}
| TWEBSITE     TEQUAL p=TSTRING        { Setup.setWebsiteDir(p)}
| TFINDCMD     TEQUAL c=TSTRING        { Setup.setFindCmd(c)}
| TFINDCHILD   TEQUAL c=TInt           { Setup.setFindChild(c)}
| TSPFLAGS     TEQUAL f=TSTRING        { Setup.setSPFlags(f)}
| TCPUCORE     TEQUAL c=TInt           { Setup.setCPUcore(c)}


experience:
   TEXPERIENCE name=TId descExperience=objects    {Setup.addExp name (descExperience)}  


objects:
 |TON TPATTERN name=TId patterns= list(patternobj) TWITH TPROJECT name2=TId projects= list(projectsub) 
     {(Ast_config.ObjPatt((Ast_config.ExpPattern(name))::patterns),Ast_config.ObjProj((Ast_config.ExpProject(name2))::projects))}
 |TON TPROJECT name=TId projects= list (projectobj) TWITH TPATTERN name2=TId patterns= list(patternsub) 
     {(Ast_config.ObjProj((Ast_config.ExpProject(name))::projects),Ast_config.ObjPatt((Ast_config.ExpPattern(name2))::patterns))}

patternobj:
 TON TPATTERN name=TId {Ast_config.ExpPattern(name)}

projectobj:
 TON TPROJECT name=TId {Ast_config.ExpProject(name)}


patternsub:
 TWITH TPATTERN name=TId {Ast_config.ExpPattern(name)}
 

 
 
projectsub: 
 TWITH TPROJECT name=TId {Ast_config.ExpProject(name)}








attr:
  TCOLOR         TEQUAL r=float v=float b=float    { Ast_config.Color(r,v,b) }
| TCORREL        TEQUAL TNONE                      { Ast_config.Correl("none") }
| TCORREL        TEQUAL dft=TId                    { Ast_config.Correl(dft) }
| TCLEANCOLOR    TEQUAL r=float v=float b=float    { Ast_config.CleanColor(r,v,b)}
| TPATTERN       TEQUAL dft=TId                    { Ast_config.DftPattern(dft)}
| TPATTERNCOLOR  TEQUAL r=float v=float b=float    { Ast_config.PatternColor(r,v,b)}
| TDATA          TEQUAL e=expression               { Ast_config.Data(e)}
| TDIR           TEQUAL d=path                     { Setup.setDir d;Ast_config.Dir(d)}
| TSUBDIR        TEQUAL d=path                     { Ast_config.SubDir(d)}
| TFACTOR        TEQUAL f=float                    { Ast_config.Factor(f)}
| TFILE          TEQUAL f=TSTRING
    { if f = "" then Ast_config.File(None) else Ast_config.File(Some f) }
| TFILE          TEQUAL TNONE                      { Ast_config.File(None)}
| TFILENAME      TEQUAL b=TBOOL                    { Ast_config.Filename(b)}
| TFOOTER        TEQUAL l=TSTRING                  { Ast_config.Footer(l)}
| TFORMAT        TEQUAL dft=TId                    { Ast_config.Format(dft) }
| TINFO          TEQUAL b=TBOOL                    { Ast_config.Info(b)}
| TLEGEND        TEQUAL l=TSTRING                  { Ast_config.Legend(l)}
| TXLEGEND       TEQUAL l=TSTRING                  { Ast_config.XLegend(to_jgraph_fmt l)}
| TYLEGEND       TEQUAL l=TSTRING                  { Ast_config.YLegend(to_jgraph_fmt l)}
| TYLEGENDFACTOR TEQUAL f=TId                      { Ast_config.YLegendFactor(f)}
| TLINESTYLE     TEQUAL TNONE                      { Ast_config.LineType("none")}
| TLINESTYLE     TEQUAL s=TId                      { Ast_config.LineType(s)}
| TMARKTYPE      TEQUAL TNONE                      { Ast_config.MarkType("none")}
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
| TVMIN          TEQUAL v=TSTRING                  { Ast_config.VMin(v)}
| TSCM           TEQUAL v=TSTRING                  { Ast_config.SCM(v)}
| TSPFLAGS       TEQUAL f=TSTRING                  { Ast_config.SPFlags(f)}
| TSORT          TEQUAL b=TBOOL                    { Ast_config.Sort(b)}
| TSIZE          TEQUAL x=float y=float            { Ast_config.Size(x,y)}

attrs:
  TLCB atts=list(attr) TRCB {atts}

version:
  TLPAR name=TSTRING TCOMMA d=date  size=suite TRPAR {
    let count = List.length (Str.split (Str.regexp_string (Str.quote "/")) name) in if size=0 then let size=Compute_size_and_date.get_size (!Setup.projectsdir^"/"^(!Setup.dir)^"/"^name) in 
      let affiche_vers= Printf.printf"(\"%s\",%s,%d)\n" name (Misc.string_of_date d) size in
      (count, (name, d, size)) else 
        let affiche_vers = Printf.printf"(\"%s\",%s,%d)\n" name (Misc.string_of_date d) size in
        (count, (name, d, size))
  }

suite:
  |TCOMMA size=TInt {size}
  | {0 }

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

graph:
  TGRAPH name=path TLCB atts=list(attr) cs=list(curve) TRCB  { Setup.addGph name (atts,Ast_config.Curves(cs)) }
| TGRAPH name=path TLCB atts=list(attr) gs=nonempty_list(group) TRCB  { Setup.addGph name (atts,Ast_config.Groups(gs)) }

curve:
  TCURVE TPROJECT p=TId            { (Some p,None  ,   [], Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId atts=attrs { (Some p,None  , atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPATTERN d=TId            { (None  ,Some d,   [], Misc.getpos $startpos($1) $endofs) }
| TCURVE TPATTERN d=TId atts=attrs { (None  ,Some d, atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId TPATTERN d=TId atts=attrs
    { (Some p, Some d, atts, Misc.getpos $startpos($1) $endofs) }
| TCURVE TPROJECT p=TId TPATTERN d=TId
    { (Some p, Some d, [], Misc.getpos $startpos($1) $endofs) }
| TCURVE name=TSTRING atts=attrs
    {(None, None, Ast_config.Legend(name)::atts,Misc.getpos $startpos($1) $endofs)}
| TEMPTY TCURVE
    { (None, None, [], Misc.getpos $startpos($1) $endofs) }

group:
  TGROUP name=TSTRING TLCB cs=list(curve) TRCB { Ast_config.GrpCurve(name,cs)}
| TGROUP TPATTERN d=TId                        { Ast_config.GrpPatt(d, Misc.getpos $startpos($1) $endofs)}

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
| TCOCCI                   { "patterns"      }
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
| TFINDCMD                 { "findcmd"       }
| TFINDCHILD               { "findchild"     }
| TFORMAT                  { "format"        }
| TPATTERNCOLOR            { "occurcolor"    }
| TPREFIX                  { "prefix"        }
| TPROJECT                 { "project"       }
| TPROJECTS                { "projects"      }
| TRATIO                   { "ratio"         }
| TRESULTS                 { "results"       }
| TSIZE                    { "size"          }
| TSORT                    { "sort"          }
| TSPFLAGS                 { "flags"         }
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
