type age = int ;;
type identity = string * age ;;
type myBool = T | F;; 
type myBool' = T' of unit | F' of unit ;;
T;;
T'();;

let myBool2bool b = match b with
    T -> true 
  | F -> false 
;;

myBool2bool T;; 

let bool2myBool b = match b with 
    true -> T 
  | false -> F 
;; 

bool2myBool true;;

type nat = Z | S of nat;;

let zero = Z;; 
let one = S Z;; 
let two = S (S Z);; 
let three = S (S (S Z));;

let nonzero n = match n with
    Z -> false 
  | S _ -> true 
;; 

nonzero zero;; 
nonzero three;; 

let rec nat2int n = match n with
    Z -> 0 
  | S x -> 1+(nat2int x) 
;; 
nat2int zero;; 
nat2int one;; 
nat2int two;; 
nat2int three;; 

let rec addnat m n = match m with 
    Z -> n (* 0+n = n *)
  | S x -> S (addnat x n) (* (1+x) + n = 1 + (x+n) *)
;; 
addnat zero three;; 
addnat three zero;; 
addnat one two;; 
addnat two one;; 

let rec multnat m n = match m with 
    Z -> Z (* 0 * n = 0 *) 
  | S x -> addnat n (multnat x n) (* (1+x) * n = n + (x*n) 
                                  *) 
;; 
multnat zero two;; 
multnat two zero;; 
multnat one three;; 
multnat three one;; 
multnat three two;; 

let rec expnat m n = match n with
    Z -> (S Z) (* m^0 = 1 *) 
  | S x -> multnat m (expnat m x) (* m^(1+x) = m * (m^x) *) 
;; 
expnat zero one;; 
expnat zero zero;; 
expnat zero three;; 
expnat three zero;; 
expnat two three;; 
expnat two one;; 
expnat three two;; 

type exp = Num of int | Bl of myBool 
         | Plus of exp * exp | Times of exp * exp 
         | And of exp * exp | Or of exp * exp | Not of exp 
         | Eq of exp * exp | Gt of exp * exp 
;;

let test1 = Plus (Times (Num 3, Num 4), 
                  Times (Num 5, Num 6));; 

let test2 = Or (Not (Bl T), 
                And (Bl T, 
                     Or(Bl F, 
                        Bl T)));;

let test3 = Gt (Times (Num 5, Num 6), 
                (Times (Num 3, Num 4)));; 

let test4 = And (Eq(test1, Num 42), Not test3);;

let rec ht e = match e with
    Num n -> 0 
  | Bl b -> 0 
  | Plus (e1, e2) -> 1 + (max (ht e1) (ht e2)) 
  | Times (e1, e2) -> 1 + (max (ht e1) (ht e2)) 
  | And (e1, e2) -> 1 + (max (ht e1) (ht e2)) 
  | Or (e1, e2) -> 1 + (max (ht e1) (ht e2)) 
  | Not e1 -> 1 + (ht e1) 
  | Eq (e1, e2) -> 1 + (max (ht e1) (ht e2)) 
  | Gt(e1, e2) -> 1 + (max (ht e1) (ht e2)) 
;; 

let rec size e = match e with
    Num n -> 1 
  | Bl b -> 1 
  | Plus (e1, e2) -> 1 + (size e1) + (size e2) 
  | Times (e1, e2) -> 1 + (size e1) + (size e2) 
  | And (e1, e2) -> 1 + (size e1) + (size e2) 
  | Or (e1, e2) -> 1 + (size e1) + (size e2) 
  | Not e1 -> 1 + (size e1) 
  | Eq (e1, e2) -> 1 + (size e1) + (size e2) 
  | Gt(e1, e2) -> 1 + (size e1) + (size e2) 
;; 

let h1 = ht test1;;
let h2 = ht test2;; 
let h3 = ht test3;; 
let h4 = ht test4;; 
let s1 = size test1;; 
let s2 = size test2;; 
let s3 = size test3;; 
let s4 = size test4;; 

type values = N of int | B of bool ;; 
let rec eval e = match e with
    Num n -> N n 
  | Bl b -> B (myBool2bool b) 
  | Plus (e1, e2) -> let N n1 = (eval e1) 
      and N n2 = (eval e2) 
      in N (n1 + n2) 
  | Times (e1, e2) -> let N n1 = (eval e1) and N n2 = (eval e2) 
      in N (n1 * n2) 
  | And (e1, e2) -> let B b1 = (eval e1) 
      and B b2 = (eval e2) 
      in B (b1 && b2) 
  | Or (e1, e2) -> let B b1 = (eval e1) 
      and B b2 = (eval e2) 
      in B (b1 || b2) 
  | Not e1 -> let B b1 = (eval e1) in B (not b1) 
  | Eq (e1, e2) -> let N n1 = (eval e1) 
      and N n2 = (eval e2) 
      in B (n1 = n2) 
  | Gt(e1, e2) -> let N n1 = (eval e1) 
      and N n2 = (eval e2) 
      in B (n1 > n2) 
;;

let v1 = eval test1;; 
let v2 = eval test2;; 
let v3 = eval test3;; 
let v4 = eval test4;; 

type opcode = LDN of int | LDB of bool 
            | PLUS | TIMES | AND | OR | NOT | EQ | GT;;

let rec compile e = match e with
    Num n -> [ LDN n ] 
  | Bl b -> [LDB (myBool2bool b) ] (* Constants *) 
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS] 
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES] 
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND] 
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR] 
  | Not e1 -> (compile e1) @ [NOT] 
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ] 
  | Gt(e1, e2) -> (compile e1) @ (compile e2) @ [GT] 
;; 
let c1 = compile test1;; 
let c2 = compile test2;; 
let c3 = compile test3;; 
let c4 = compile test4;; 

exception Stuck of (values list * opcode list);; 
let rec stkmc s c = match s, c with
    v::_, [ ] -> v (* no more opcodes, return top *) 
  | s, (LDN n)::c' -> stkmc ((N n)::s) c' 
  | s, (LDB b)::c' -> stkmc ((B b)::s) c' 
  | (N n2)::(N n1)::s', PLUS::c' -> stkmc (N(n1+n2)::s') c' 
  | (N n2)::(N n1)::s', TIMES::c' -> stkmc (N(n1*n2)::s') c' 
  | (B b2)::(B b1)::s', AND::c' -> stkmc (B(b1 && b2)::s') c' 
  | (B b2)::(B b1)::s', OR::c' -> stkmc (B(b1 || b2)::s') c' 
  | (B b1)::s', NOT::c' -> stkmc (B(not b1)::s') c' 
  | (N n2)::(N n1)::s', EQ::c' -> stkmc (B(n1 = n2)::s') c' 
  | (N n2)::(N n1)::s', GT::c' -> stkmc (B(n1 > n2)::s') c' 
  | _, _ -> raise (Stuck (s, c)) 
;;

let w1 = stkmc [ ] (c1) ;; 
let w2 = stkmc [ ] (c2) ;; 
let w3 = stkmc [ ] (c3) ;; 
let w4 = stkmc [ ] (c4) ;;