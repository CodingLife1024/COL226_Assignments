(* Token type definition *)
type token =
  | IDENTIFIER of string
  | KEYWORD of string
  | BOOL_OP of string
  | BOOL_CONST of bool
  | ARITH_OP of string
  | INT_CONST of int
  | COMP_OP of string
  | STRING_OP of string
  | STRING_CONST of string
  | PAREN of string
  | COMMA

(* Regular expression definitions *)
let identifier_regex = "[a-z_][a-zA-Z0-9'_]*"
let keyword_regex = "(if|then|else)"
let bool_op_regex = "(and|or|not)"
let bool_const_regex = "(true|false)"
let arith_op_regex = "(\+|\-|\*|\/)"
let int_const_regex = "[1-9][0-9]*"
let comp_op_regex = "(=|!=|<|<=|>|>=)"
let string_op_regex = "(^|@)"
let string_const_regex = "\"[^\"]*\""
let paren_regex = "(\(|\))"
let comma_regex = ","

(* Function to tokenize input *)
let tokenize input =
  let rec tokenize_helper input tokens =
    match input with
    | "" -> List.rev tokens
    | _ ->
      let match_regex regex =
        let regexp = Str.regexp ("^" ^ regex) in
        try
          let matched_text = Str.matched_string input in
          Some (matched_text, Str.string_after input (Str.match_end ()))
        with Not_found -> None
      in
      match
        List.find_opt Option.is_some
          [ match_regex identifier_regex, IDENTIFIER;
            match_regex keyword_regex, KEYWORD;
            match_regex bool_op_regex, BOOL_OP;
            match_regex bool_const_regex, (fun s -> BOOL_CONST (s = "true"));
            match_regex arith_op_regex, ARITH_OP;
            match_regex int_const_regex, (fun s -> INT_CONST (int_of_string s));
            match_regex comp_op_regex, COMP_OP;
            match_regex string_op_regex, STRING_OP;
            match_regex string_const_regex, (fun s -> STRING_CONST (String.sub s 1 (String.length s - 2)));
            match_regex paren_regex, (fun s -> PAREN s);
            match_regex comma_regex, (fun _ -> COMMA) ]
      with
      | Some (matched_text, rest) ->
        let token_type = Option.get_exn (List.assoc_opt matched_text tokens) in
        tokenize_helper rest ((token_type matched_text) :: tokens)
      | None -> failwith "Invalid token"
  in
  tokenize_helper input []

(* Example usage *)
let input = "if x + y * 3 = 10 then \"true\" else \"false\""
let tokens = tokenize input
let () = List.iter (fun t -> Printf.printf "%s\n" (match t with IDENTIFIER s -> "Identifier: " ^ s | _ -> "Other")) tokens
