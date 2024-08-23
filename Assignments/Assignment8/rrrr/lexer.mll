(* lexer.mll *)

{
  open Parser;;(* Import the Parser module *)
  exception InvalidToken of char ;; (* Define an exception for invalid tokens *)
}

(* Regular expression definitions *)
let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_'] (* Alphanumeric characters and underscore *)
let var = ['A'-'Z'](alpha_num*)(* Variable names starting with an uppercase letter *)
let cons = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"") (* Constructors starting with a lowercase letter or strings in double quotes *)
let sp = [' ' '\t' '\n']+(* Whitespace characters *)
let number = '0'|['1'-'9']['0'-'9']* (* Numbers, including integers *)

(* Rule for reading input *)
rule read = parse
    eof                   {EOF}(* End of file *)
  | sp                    {read lexbuf}(* Whitespace, continue reading *)
  | var as v              {VAR(v)} (* Variable *)
  | cons as c             {CONS(c)}(* Constructor *)
  | number as n           {NUM(int_of_string n)} (* Number *)
  | '('                   {LP}(* Left parenthesis *)
  | ')'                   {RP}(* Right parenthesis *)
  | '['                   {LB} (* Left bracket *)
  | ']'                   {RB}(* Right bracket *)
  | ','                   {COMMA}(* Comma *)
  | '='                   {EQ}(* Equals *)
  | '+'                   {PLUS}(* Plus *)
  | '-'                   {MINUS}(* Minus *)
  | '*'                   {MULT}(* Multiply *)
  | '/'                   {DIV}(* Divide *)
  | '>'                   {GT}(* Greater than *)
  | '<'                   {LT} (* Less than *)
  | "\\="                 {NOT_EQ}(* Not equal *)
  | '|'                   {PIPE} (* Pipe *)
  | '!'                   {CUT}(* Cut *)
  | '.'                   {ENDL} (* End of line *)
  | ":-"                  {COND}(* Condition operator *)
  | '%'                   {single_line_comment lexbuf} (* Single-line comment *)
  | "/*"                  {multi_line_comment 0 lexbuf} (* Multi-line comment *)
  | _ as s                {raise (InvalidToken s)}(* Invalid token *)

(* Rule for single-line comments *)
and single_line_comment = parse
    eof                   {EOF}(* End of file *)
  | '\n'                  {read lexbuf}(* Newline, continue reading *)
  |   _                   {single_line_comment lexbuf} (* Continue comment *)

(* Rule for multi-line comments *)
and multi_line_comment depth = parse
    eof                   {failwith "Syntax error: End of file in /* ... */ comment"} (* Error if end of file reached *)
  | "*/"                  {if depth = 0 then read lexbuf else multi_line_comment (depth-1) lexbuf} (* End of multi-line comment *)
  | "/*"                  {multi_line_comment (depth+1) lexbuf} (* Nested multi-line comment *)
  |  _                    {multi_line_comment depth lexbuf} (* Continue multi-line comment *)
