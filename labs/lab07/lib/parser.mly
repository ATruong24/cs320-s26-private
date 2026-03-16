%token LPAREN
%token RPAREN
%token<string> ATOM
%token EOF

%start<Ast.sexpr> prog

%%

prog:
  | EOF { e }
