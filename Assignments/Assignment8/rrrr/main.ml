(* main.ml *)

(* Import the lexer, parser, and interpreter modules *)
open Lexer;;
open Parser;;
open Interpreter;;

(* Check if the input file is provided *)
if Array.length Sys.argv < 2 then begin
  print_string "Wrong Input...\n";
  exit 0;
end;;

(* Check if only one input file is provided *)
if Array.length Sys.argv > 2 then begin
  print_string "Only 2 Arguments Permitted...\n";
  exit 0;
end;;

(* Open the input file *)
let fstream = open_in Sys.argv.(1);;

(* Parse the initial program *)
let init_prog = Parser.program Lexer.read (Lexing.from_channel fstream);;

(* Check the validity of the initial program *)
let _ = checkProgram init_prog;;

(* Modify the initial program *)
let prog = modifyInitialProg init_prog 1;;

(* Print a message indicating that the program is loaded *)
print_string "Program loaded\n";;

(* Main loop *)
try
  while true do
    let line = read_line () in
    (* Checking if the input is 'halt.' to terminate the program *)
    if line = "halt." then exit 0
    else try
      (* Parsing the goal *)
      let g = Parser.goal Lexer.read (Lexing.from_string line) in
      (* Interpreting the goal *)
      match interpret_goal prog g with
        | true, _ -> print_string "true.\n"
        | false, _ -> print_string "false.\n"
    with e ->
      (* Handling exceptions *)
      Printf.printf "%s\n" (Printexc.to_string e)
  done
with _ -> print_string "Terminated...\n"