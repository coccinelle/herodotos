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
   TId sep vs=separated_nonempty_list(sep,TId) EOL
    es=list(entry) EOF { (vs,es) }

entries:
   es=list(entry) EOF                { es     }

entry:
   id=TId sep states=separated_nonempty_list(sep,state_info) EOL
    { (id,states)     }

state_info:
   TTrue                             { Ast_exist.True     }
 | TFalse                            { Ast_exist.False    }
 | i=TInt                            { Ast_exist.Size (i) }

%inline sep:
 TSC {}
