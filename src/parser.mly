%{
open Lang
%}

%token FUN TO LET REC EQ IN APP
%token LPAR RPAR SC
%token LACC RACC COMMA DOT
%token<string> IDENT
%token<int> INT
%token<string> STRING
%token EOF

%start prog

%type<Lang.decl list> prog
%type<Lang.decl list> decls
%type<Lang.decl> decl
%type<string list> args
%type<Lang.t> expr
%type<(string * Lang.t) list> record
%type<bool> recursive

%nonassoc INT STRING IDENT FUN LACC TO LET IN
%nonassoc APP
%left DOT LPAR
%%

prog:
  | decls EOF { $1 }

decls:
  | decl SC SC decls { $1::$4 }
  | { [] }

decl:
  | LET recursive IDENT args EQ expr { $2, $3, abs $4 $6 }

args:
  | IDENT args { $1::$2 }
  | { [] }

expr:
  | INT { Int $1 }
  | STRING { String $1 }
  | IDENT { Var $1 }
  | FUN IDENT TO expr { Abs ($2, $4) }
  (* Precedence of application is tricky:
     https://ptival.github.io/2017/05/16/parser-generators-and-function-application/
     *)
  | expr expr %prec APP { App ($1, $2) }
  | decl IN expr { let r, x, t = $1 in Let (r, x, t, $3) }
  | LACC record RACC { Record $2 }
  | expr DOT IDENT { Field ($1, $3) }
  | LPAR expr RPAR { $2 }

record:
  | { [] }
  | IDENT EQ expr { [$1,$3] }
  | IDENT EQ expr COMMA record { ($1,$3)::$5 }

recursive:
  | REC { true }
  | { false }
