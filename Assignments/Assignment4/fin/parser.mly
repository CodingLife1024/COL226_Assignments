// parser.mly

%token <int> INT
%token <bool> BOOL
%token ADD MUL SUB DIV
%token LPAREN RPAREN
%token TRUE FALSE
%token AND OR NOT
%token GREATER LESSER EQUAL
%token IF THEN ELSE ELIF
%token EOF

%start main
%type <int> main
%type <int> expr term factor
%type <bool> bool_expr bool_term bool_factor

%%

main:
  | expr EOF
    { $1 }

expr:
  | expr ADD term
    { $1 + $3 }
  | expr SUB term
    { $1 - $3 }
  | term
    { $1 }

term:
  | term MUL factor
    { $1 * $3 }
  | term DIV factor
    { $1 / $3 }
  | factor
    { $1 }

factor:
  | INT
    { $1 }
  | SUB factor
    { -$2 }
  | LPAREN expr RPAREN
    { $2 }
  | bool_expr
    { if $1 then 1 else 0 }
  | IF bool_expr THEN statement ELSE statement
    { if $2 then $4 else $6 }

statement:
  | expr
    { $1 }
  | IF bool_expr THEN statement ELSE statement
    { if $2 then $4 else $6 }

bool_expr:
  | TRUE
    { true }
  | FALSE
    { false }
  | NOT bool_factor
    { not $2 }
  | LPAREN bool_expr RPAREN
    { $2 }
  | bool_expr AND bool_term
    { $1 && $3 }
  | bool_expr OR bool_term
    { $1 || $3 }
  | expr GREATER expr
    { $1 > $3 }
  | expr LESSER expr
    { $1 < $3 }
  | expr EQUAL expr
    { $1 = $3 }

bool_term:
  | bool_factor
    { $1 }

bool_factor:
  | TRUE
    { true }
  | FALSE
    { false }
  | NOT bool_factor
    { not $2 }
  | LPAREN bool_expr RPAREN
    { $2 }