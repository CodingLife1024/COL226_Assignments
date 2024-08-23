type exp =
  | Num of int
  | Bool of bool
  | Var of string
  | Plus of exp * exp
  | Times of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Eq of exp * exp
  | Gt of exp * exp

type typ =
  | IntT
  | BoolT

let rec hastype (env : (string * typ) list) (e : exp) : typ =
  match e with
  | Num _ -> IntT
  | Bool _ -> BoolT
  | Var x -> (try List.assoc x env with Not_found -> failwith ("Variable " ^ x ^ " not found in environment"))
  | Plus (e1, e2) | Times (e1, e2) ->
      (match (hastype env e1, hastype env e2) with
       | (IntT, IntT) -> IntT
       | _ -> failwith "Type error in arithmetic expression")
  | And (e1, e2) | Or (e1, e2) ->
      (match (hastype env e1, hastype env e2) with
       | (BoolT, BoolT) -> BoolT
       | _ -> failwith "Type error in boolean expression")
  | Not e1 ->
      (match hastype env e1 with
       | BoolT -> BoolT
       | _ -> failwith "Type error in boolean expression")
  | Eq (e1, e2) | Gt (e1, e2) ->
      (match (hastype env e1, hastype env e2) with
       | (IntT, IntT) -> BoolT
       | _ -> failwith "Type error in comparison expression")

(* Example usage *)
let env = [("x", IntT); ("y", IntT)]
let exp1 = Plus (Var "x", Var "y")
let exp2 = Eq (Var "x", Var "y")

(* Helper functions to convert expressions and types to strings for printing *)
let rec string_of_exp = function
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var x -> x
  | Plus (e1, e2) -> Printf.sprintf "(%s + %s)" (string_of_exp e1) (string_of_exp e2)
  | Times (e1, e2) -> Printf.sprintf "(%s * %s)" (string_of_exp e1) (string_of_exp e2)
  | And (e1, e2) -> Printf.sprintf "(%s && %s)" (string_of_exp e1) (string_of_exp e2)
  | Or (e1, e2) -> Printf.sprintf "(%s || %s)" (string_of_exp e1) (string_of_exp e2)
  | Not e1 -> Printf.sprintf "(!%s)" (string_of_exp e1)
  | Eq (e1, e2) -> Printf.sprintf "(%s = %s)" (string_of_exp e1) (string_of_exp e2)
  | Gt (e1, e2) -> Printf.sprintf "(%s > %s)" (string_of_exp e1) (string_of_exp e2)

and string_of_typ = function
  | IntT -> "int"
  | BoolT -> "bool"
    
let () =
  let result1 = hastype env exp1 in
  let result2 = hastype env exp2 in
  Printf.printf "Type of %s: %s\n" (string_of_exp exp1) (string_of_typ result1);
  Printf.printf "Type of %s: %s\n" (string_of_exp exp2) (string_of_typ result2)
