(* lexer.mll *)

{
  open Parser
}

rule token = parse
    [' ' '\t' '\n']
        { token lexbuf }
    | ['.']
        { EOF }
    | ['+']
        { ADD }
    | ['*']
        { MUL }
    | ['-']
        { SUB }
    | ['/']
        { DIV }
    | "and" | "&&"
        { AND }
    | "or" | "||"
        { OR }
    | "not"
        { NOT }
    | "true"
        { TRUE }
    | "false"
        { FALSE }
    | ['>']
        { GREATER }
    | ['<']
        { LESSER }
    | "=="
        { EQUAL }
    | ['0'-'9']+ as lxm
        { INT(int_of_string lxm) }
    | ['(']
        { LPAREN }
    | [')']
        { RPAREN }
    | "if"
        { IF }
    | "elif"
        { ELIF }
    | "then"
        { THEN }
    | "else"
        { ELSE }
    | _ as c
        { failwith ("Unexpected character: " ^ Char.escaped c) }
    | eof
        { EOF }
