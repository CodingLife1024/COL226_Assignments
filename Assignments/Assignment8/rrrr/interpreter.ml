(* interpreter.ml *)

(* Define various types used in the interpreter *)

type variable = string
type symbol = string
type signature = (symbol * int) list
type term = V of variable | Num of int | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list

(* Define exceptions *)

exception NOT_UNIFIABLE
exception NotFound
exception InvalidProgram
exception NotPossible

(* Utility functions *)

(* Check if an element exists in a list *)
let rec exists x y = match y with
    [] -> false
  | z::ys -> (x = z) || (exists x ys)
;;

(* Fold function over a list *)
let rec foldl f e l = match l with
    [] -> e
  | x::xs -> foldl f (f e x) xs
;;

(* Map function over a list *)
let rec map f l = match l with
    [] -> []
  | x::xs -> (f x)::map f xs
;;

(* Combine two lists into a list of tuples *)
let rec combine l1 l2 = match l1 with
    [] -> []
  | x::xs -> (x, (List.hd l2))::combine xs (List.tl l2)
;;

(* Union of two lists *)
let rec union l1 l2 = match l1 with
    [] -> l2
  | x::xs -> if (exists x l2) then union xs l2
             else x::(union xs l2)
;;

(* Check if a program is valid *)
let rec checkProgram (prog:program): bool = match prog with
    [] -> true
  | (F(H(a)))::xs | (R(H(a), _))::xs -> match a with
          A("_eq", _) | A("_not_eq", _) | A("_cut", _)
        | A(">", _) | A("<", _)-> raise InvalidProgram
        | _ -> checkProgram xs
;;

(* Modify term by appending a number to variable names *)
let rec modifyTerm (i:int) (t:term): term = match t with
    V(v) -> V((string_of_int i) ^ v)
  | Node(s, l) -> Node(s, map (modifyTerm i) l)
  | _ -> t
;;

(* Modify atom by appending a number to variable names *)
let rec modifyAtom (i:int) (a:atom): atom = match a with
  A(s, l) -> A(s, map (modifyTerm i) l)
;;

(* Modify clause by modifying the atom *)
let rec modifyClause (cl:clause) (i:int): clause = match cl with
    F(H(a)) -> F(H(modifyAtom i a))
  | R(H(a), B(l)) -> R(H(modifyAtom i a), B(map (modifyAtom i) l))
;;

(* Modify initial program by modifying each clause *)
let rec modifyInitialProg (prog:program) (i:int): program = match prog with
    [] -> []
  | cl::ps -> (modifyClause cl i)::modifyInitialProg ps (i+1)
;;

(* Modify the program by removing clauses with the same symbol as the given atom *)
let rec modifyProg2 (prog:program) (A(s, _): atom): program = match prog with
    [] -> []
  | cl::ps -> match cl with F(H(A(s', _))) | R(H(A(s', _)), _) ->
                if s = s' then (modifyClause cl 0)::modifyProg2 ps (A(s, []))
                else cl::modifyProg2 ps (A(s, []))
;;

(* Get variables in a term *)
let rec vars_term (t:term): variable list =
  match t with
      V(v) -> [v]
    | Node(s, l) -> foldl union [] (map vars_term l)
    | _ -> []
;;

(* Get variables in a atom *)
let vars_atom (A(s, l): atom): variable list = vars_term (Node(s, l))
;;

(* Get variables in a goal *)
let rec vars_goal (G(g): goal): variable list = foldl union [] (map vars_atom g)
;;

(* Substitute terms in substitution *)
let rec subst (s:substitution) (t:term): term =
  match t with
      Node(s', l) -> Node(s', map (subst s) l)
    | Num(_) -> t
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

(* Substitute terms in atom *)
let rec subst_atom (s:substitution) (A(s', l): atom): atom = A(s', map (subst s) l)
;;

(* Check if a variable occurs in a term *)
let rec variableInTerm (v:variable) (t:term): bool =
  match t with
      V(x) -> x = v
    | Node(s, l) -> foldl (||) false (map (variableInTerm v) l)
    | _ -> false
;;

(* Compose two substitutions *)
let compose (s1:substitution) (s2:substitution): substitution =
  let f s x = (fst x, subst s (snd x)) in (map (f s2) s1) @ s2
;;

(* Most General Unifier (mgu) for two terms *)
let rec mgu_term (t1:term) (t2:term): substitution =
  match (t1, t2) with
      (V(x), V(y)) -> if x = y then []
                      else [(x, V(y))]
    | (V(x), Node(_, _)) -> if variableInTerm x t2 then raise NOT_UNIFIABLE
                            else [(x, t2)]
    | (Node(_, _), V(y)) -> if variableInTerm y t1 then raise NOT_UNIFIABLE
                            else [(y, t1)]
    | (Num(n1), Num(n2)) -> if n1 = n2 then [] else raise NOT_UNIFIABLE
    | (Num(n1), V(x)) -> [(x, t1)]
    | (V(x), Num(n2)) -> [(x, t2)]
    | (Node(s1, l1), Node(s2, l2)) ->
        if s1 <> s2 || (List.length l1 <> List.length l2) then raise NOT_UNIFIABLE
        else
          let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
          foldl f [] (combine l1 l2)
    | _ -> raise NOT_UNIFIABLE
;;

(* Most General Unifier (mgu) for two atoms *)
let mgu_atom (A(s1, l1): atom) (A(s2, l2): atom): substitution = mgu_term (Node(s1, l1)) (Node(s2, l2))
;;

(* Print a list of terms *)
let rec print_term_list (tl:term list) = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ",";
      print_term_list tls;
    )

(* Print the body of a list *)
and print_list_body (t:term) = match t with
    Node("_empty_list", []) -> Printf.printf ""
  | Node("_list", [t1; Node("_empty_list", [])]) -> print_term t1
  | Node("_list", [t1; t2]) -> (
      print_term t1;
      Printf.printf ",";
      print_list_body t2;
    )
  | _ -> raise NotPossible

(* Print a term *)
and print_term (t:term) = match t with
    V(v) -> Printf.printf " %s " v
  | Node("_empty_list", []) -> Printf.printf " [] "
  | Node(s, []) -> Printf.printf " %s " s
  | Node("_list", _) -> (
      Printf.printf " [";
      print_list_body t;
      Printf.printf "] ";
    )
  | Node(s, l) -> (
      Printf.printf " %s ( " s;
      print_term_list l;
      Printf.printf " ) ";
    )
  | Num(n) -> Printf.printf " %d " n
;;

(* Get a solution from a substitution *)
let rec getSolution (unif:substitution) (vars:variable list) = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NotFound
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getSolution unif vs
      with NotFound -> getSolution unif vs
;;

(* Print a solution *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let rec printSolution (unif:substitution) = match unif with
    [] -> Printf.printf "true. "
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printSolution xs;
    )
;;

(* Solve unifiable atoms *)
let solve_atom_atom (a1:atom) (a2:atom) (unif:substitution): substitution =
  compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))
;;

(* Solve unifiable terms *)
let solve_term_term (t1:term) (t2:term) (unif:substitution): substitution =
  compose unif (mgu_term (subst unif t1) (subst unif t2))
;;

(* Simplify a term *)
let rec simplify_term (t:term): term = match t with
    Num(_) -> t
  | Node("+", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 + n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("-", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 - n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("*", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 * n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("/", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 / n2)
        | _ -> raise NOT_UNIFIABLE
      )
  | _ -> t
;;

(* Evaluate atoms *)
let eval (a:atom) (unif:substitution): substitution = match a with
    A("_eq", [t1; t2])
  | A("_not_eq", [t1; t2]) -> compose unif (mgu_term (simplify_term (subst unif t1)) (simplify_term (subst unif t2)))
  | A(">", [t1; t2]) -> (
        match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
            (Num(n1), Num(n2)) -> if n1 > n2 then unif else raise NOT_UNIFIABLE
          | _ -> raise NOT_UNIFIABLE
    )
  | A("<", [t1; t2]) -> (
      match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
          (Num(n1), Num(n2)) -> if n1 < n2 then unif else raise NOT_UNIFIABLE
        | _ -> raise NOT_UNIFIABLE
    )
  | _ -> unif
;;

(* Solve a goal *)
let rec solve_goal (prog:program) (g:goal) (unif:substitution) (vars:variable list): (bool * substitution) =
  match g with
      G([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        let choice = ref (get1char()) in
        while(!choice <> '.' && !choice <> ';') do
          Printf.printf "\nUnknown Action: %c \nAction? " (!choice);
          flush stdout;
          choice := get1char();
        done;
        Printf.printf "\n";
        if !choice = '.' then (true, [])
        else (false, [])
      )
    | G(a::gs) -> match a with
          A("_eq", _) | A(">", _) | A("<", _) -> (
            try solve_goal prog (G(gs)) (eval a unif) vars
            with NOT_UNIFIABLE -> (false, [])
          )
        | A("_not_eq", _) -> (
            try (false, eval a unif)
            with NOT_UNIFIABLE -> solve_goal prog (G(gs)) unif vars
          )
        | A("_cut", _) -> let _ = solve_goal prog (G(gs)) unif vars in (true, [])
        | _ ->
          let new_prog = modifyProg2 prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                F(H(a')) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
              | R(H(a'), B(al)) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(al @ gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
        in iter prog
;;

let interpret_goal (prog:program) (g:goal) = solve_goal prog g [] (vars_goal g)
;;
