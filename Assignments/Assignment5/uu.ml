type symbol = string
type signature = (symbol * int) list

type tree =
  | V of symbol
  | C of { node: symbol; children: tree list }

(* ---------- *)
(* Checks the signature and arities *)
let check_sig (sig_list : signature) : bool =
  let rec check_duplicates = function

    | [] -> false
    | hd :: tl -> List.mem hd tl || check_duplicates tl
  in
  let rec check_non_negative = function
    | [] -> true
    | (_, arity) :: tl -> arity >= 0 && check_non_negative tl
  in
  not (check_duplicates sig_list) && check_non_negative sig_list

let run_tests () =
  let test_cases = [
    [("0", 0); ("1", 0); ("0", 1)];
    [("0", 0); ("1", 0); ("+", 2)]  (* Negative arity *)
  ] in

  let rec run_single_testcases = function
    | [] -> ()
    | test_signature :: remaining ->
        let result = check_sig test_signature in
        if result then
          print_endline "Test passed!"
        else
          print_endline "Test failed!";
        run_single_testcases remaining
  in

  run_single_testcases test_cases

(* Run the tests *)
let () = run_tests ()

(* ---------- *)

(* Well formed tree *)

let rec wftree (signature : signature) (tree : tree) : bool =
  let rec check_signature symbol =
    List.assoc_opt symbol signature
  in
  let rec wftree_helper tree =
    match tree with
    | V _ -> true
    | C { node; children } ->
        match check_signature node with
        | Some arity -> arity = List.length children && List.for_all wftree_helper children
        | None -> false
  in
  wftree_helper tree


let () =
  let signature = [("a", 2); ("b", 3); ("+", 2)] (* Signature: a has arity 2, b has arity 3 *) in

  let test_tree_1 = C { node = "+"; children = [(V "x"); (V "y"); (V "z")]} in (* Well-formed tree *)
  let result_1 = wftree signature test_tree_1 in
  print_endline ("Test 1: " ^ string_of_bool result_1);  (* Expected: true *)

  let test_tree_2 = C { node = "+"; children = [(V "x"); (V "y")]} in (* Well-formed tree *)
  let result_2 = wftree signature test_tree_2 in
  print_endline ("Test 2: " ^ string_of_bool result_2);  (* Expected: true *)

  let test_tree_3 = C { node = "+"; children = [(V "z"); test_tree_2]} in (* Not well-formed due to arity mismatch *)
  let result_3 = wftree signature test_tree_3 in
  print_endline ("Test 3: " ^ string_of_bool result_3);  (* Expected: false *)


(* ---------- *)

(* ht, size, vars *)

(* ---------- *)

(* mirror *)

(* ---------- *)

(* substitutions *)

type substitution = (string * tree) list

let check_subst (subst : substitution) : bool =
  let vars = List.map fst subst in
  let unique_vars = List.sort_uniq String.compare vars in
  List.length vars = List.length unique_vars

let rec subst (tree : tree) (s : substitution) : tree =
  match tree with
  | V v -> (
      match List.assoc_opt v s with
      | Some t -> t
      | None -> tree
    )
  | C { node; children } -> C { node; children = List.map (fun child -> subst child s) children }

let compose_subst (s1 : substitution) (s2 : substitution) : substitution =
  let apply_subst s (v, t) = (v, subst t s) in
  List.map (apply_subst s2) s1 @ s2

(* ---------- *)
(* mgu *)
