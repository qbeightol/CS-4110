open Ast

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var


type bindings = (var * int) list 
type status = N (*normal*) | B (*broken*) | C (*continued*)  
(* A type for stores *)
type store = (status * bindings)

(*custom exceptions:
exception Broken of store
exception Continued of store
*)

(* nevermind:
(*a wrapper type capable of holding the three kinds of expressions in IMP*)
type exp = Aexp of aexp | Bexp of bexp | Com of com
*)



(* A type for configurations *)
type configuration = (store * com) (*check if this makes sense; I might only 
need to focus on commands*)

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
  ((N,[]), c)

(*evaluates the arithmetic expression, [a], in the context provided by [bs]*)
let rec evala (bs: bindings) (a: aexp) : int =
  match a with 
  | Int n          -> n
  | Var var_name   -> List.assoc var_name bs
  | Plus (a1, a2)  -> (evala bs a1) + (evala bs a2)
  | Minus (a1, a2) -> (evala bs a1) - (evala bs a2)
  | Times (a1, a2) -> (evala bs a1) * (evala bs a2)
  | Input          -> print_string ">"; read_int ()

(*evaluates the boolean expression, [b], in the context provided by [bs]*)
let rec evalb (bs: bindings) (b: bexp) : bool = 
  match b with 
  | True               -> true 
  | False              -> false 
  | Equals (a1, a2)    -> (evala bs a1) = (evala bs a2)
  | NotEquals (a1, a2) -> (evala bs a1) <> (evala bs a2)
  | Less (a1, a2)      -> (evala bs a1) < (evala bs a2)
  | LessEq (a1, a2)    -> (evala bs a1) <= (evala bs a2)
  | Greater (a1, a2)   -> (evala bs a1) > (evala bs a2)
  | GreaterEq (a1, a2) -> (evala bs a1) >= (evala bs a2)
  | Not b              -> not (evalb bs b)
  | And (b1, b2)       -> (evalb bs b1) && (evalb bs b2)
  | Or (b1, b2)        -> (evalb bs b1) || (evalb bs b2)

(*short hand for string_of_int, used to make the printing of a expressions and  
  test command results a little cleaner*)
let soi = string_of_int


(*helper function for evalc--implements the core functionality of evaluation;
  may raise TestFailure*)
let rec evalc' (((stat: status), (bs: bindings)), (c: com)) : store = 
  match stat, c with 
  | N, Skip -> (N, bs)
  | N, Assign (var_name, a) -> (N, (var_name, (evala bs a))::bs)
  (*
  | N, Seq (Break, c2) -> evalc ((B, bs), c2)
  | N, Seq (Continue, c2) -> evalc ((C, bs), c2)
  *)
  | N, Seq (c1, c2) -> let s' = evalc' ((N, bs), c1) in evalc' (s', c2)
  | N, If (b, c1, c2) -> 
    if (evalb bs b) then evalc' ((N,bs), c1) else evalc' ((N, bs), c2)
  | N, While (b, c) -> evalc' ((N, bs), If (b, Seq(c, While (b,c)), Skip))
  | N, Print a -> print_endline (soi (evala bs a)); (N, bs)
  | N, Test (i, b) -> 
    if evalb bs b then (N, bs) else 
      let (l1, c1), (l2, c2) = i in
      let str = "TestFailed (test located between "
              ^ "line " ^ (soi l1) ^ ", character " ^ (soi c1) ^ " and "
              ^ "line " ^ (soi l2) ^ ", character " ^ (soi c2) 
      in print_endline str; raise (TestFailure str)
  | N, Break -> (B, bs)
  | N, Continue -> (C, bs)
  | B, While _ -> (N, bs)
  | B, _ -> (B, bs)
  | C, While (b, c) -> evalc' ((N, bs), If (b, Seq(c, While (b,c)), Skip))
  | C, _ -> (C, bs)

(*evaluates c; may throw IllegalBreak or IllegalContinue *)
let rec evalc (conf: configuration) : store = 
  match (evalc' conf) with
  | (N, bs) -> (N, bs)
  | (B, bs) -> raise IllegalBreak
  | (C, bs) -> raise IllegalContinue

