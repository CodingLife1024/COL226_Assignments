type token =
  | INT of (int)
  | BOOL of (bool)
  | ADD
  | MUL
  | SUB
  | DIV
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | GREATER
  | LESSER
  | EQUAL
  | IF
  | THEN
  | ELSE
  | ELIF
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
