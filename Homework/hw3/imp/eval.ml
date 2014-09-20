open Ast

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var


    
(* A type for stores *)
type store = (var * int) list

(*custom exceptions:*)
exception Broken of store
exception Continued of store

(* nevermind:
(*a wrapper type capable of holding the three kinds of expressions in IMP*)
type exp = Aexp of aexp | Bexp of bexp | Com of com
*)

(* A type for configurations *)
type configuration = (store * com) (*check if this makes sense; I might only 
need to focus on commands*)

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
  ([], c)

let soi = string_of_int

let rec evala (s: store) (a: aexp) : int= 
  match a with
  | Int n -> n
  | Var var_name -> List.assoc var_name s 
                    (*how should we handle unbound var names?*)
  | Plus (a1, a2) -> (evala s a1) + (evala s a2)
  | Minus (a1, a2) -> (evala s a1) - (evala s a2)
  | Times (a1, a2) -> (evala s a1) * (evala s a2)
  | Input -> 
    (*double check; think about how to handle malformed input*)
    print_string "> "; read_int ()

let rec evalb (s: store) (b: bexp) : bool =
  match b with
  | True -> true 
  | False -> false 
  | Equals (a1, a2) -> (evala s a1) = (evala s a2)
  | NotEquals (a1, a2) -> (evala s a1) <> (evala s a2)
  | Less (a1, a2) -> (evala s a1) < (evala s a2)
  | LessEq (a1, a2) -> (evala s a1) <= (evala s a2)
  | Greater (a1, a2) -> (evala s a1) > (evala s a2)
  | GreaterEq (a1, a2) -> (evala s a1) >= (evala s a2)
  | Not b -> not (evalb s b)
  | And (b1, b2) -> (evalb s b1) && (evalb s b2)
  | Or (b1, b2) -> (evalb s b1) || (evalb s b2)


(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
  match conf with
  | s, Skip -> s
  | s, (Assign (var_name, a)) -> (var_name, (evala s a))::s
  | s, (Seq (c1, c2)) -> let s' = evalc (s, c1) in evalc (s', c2)
  | s, (If (b, c_then, c_else)) -> 
    if evalb s b then evalc (s, c_then) else evalc (s, c_else)
  | s, (While (b, c)) ->
    (*double check*)
    if evalb s b then 
      try 
        let s' = evalc (s, c) in evalc (s', While (b, c))
      with 
        | Broken s -> s 
        | Continued s -> s 
    else s
  | s, (Print a) -> print_endline (soi (evala s a)); s (*double check*)
  | s, (Test (i,b)) -> 
    if evalb s b then s else 
      let (l1, c1), (l2, c2) = i in
      let str = "TestFailed (test located between "
              ^ "line " ^ (soi l1) ^ ", character " ^ (soi c1) ^ " and "
              ^ "line " ^ (soi l2) ^ ", character " ^ (soi c2) 
      in print_endline str; raise (TestFailure str)
  | s, Break -> raise (Broken s)
  | s, Continue -> raise (Continued s) 
