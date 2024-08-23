(* main.ml *)

let parse_and_evaluate input_channel =
  let lexbuf = Lexing.from_channel input_channel in
  let result = Parser.main Lexer.token lexbuf in
  Printf.printf "Result: %d\n" result

let () =
  match Array.length Sys.argv with
  | 2 ->
    let filename = Sys.argv.(1) in
    (try
       let input_channel = open_in filename in
       parse_and_evaluate input_channel;
       close_in input_channel
     with
     | Sys_error err -> prerr_endline ("Cannot open file: " ^ err))
  | _ -> prerr_endline "Usage: ./main.native <filename>"
