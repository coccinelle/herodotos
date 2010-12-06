%{

%}

%token EOF EOL
%token TTrue TFalse TSC
%token<string> TId
%token<int> TInt

%start main
%type <Ast_exist.matrix> main

%start entries
%type <Ast_exist.entry list> entries

%%

main:
   TId sep vs=separated_nonempty_list(sep,vid) EOL
    es=list(entry) list(EOL) EOF
    { (
      List.filter
	(fun vname ->
	if vname = "" then
	  false
	else
	  true
	)
	vs
	,es
      )
    }

vid:
   id=TId {id}
 |        {""}

entries:
   es=list(entry) EOF                { es     }

entry:
   id=TId sep states=separated_nonempty_list(sep,state_info) EOL
    { (id,
       List.filter
	 (fun s ->
	   match s with
	       Ast_exist.Empty -> false
	     | _ -> true
	 ) states
      )
    }

state_info:
                                     { Ast_exist.Empty    }
 | TTrue                             { Ast_exist.True     }
 | TFalse                            { Ast_exist.False    }
 | i=TInt                            { Ast_exist.Size (i) }

%inline sep:
 TSC {}
