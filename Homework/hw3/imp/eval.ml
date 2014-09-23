open Ast

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var


    
(* A type for stores *)
type store = (var * int) list

(*custom exceptions:
exception Broken of store
exception Continued of store
*)

(* nevermind:
(*a wrapper type capable of holding the three kinds of expressions in IMP*)
type exp = Aexp of aexp | Bexp of bexp | Com of com
*)

type status = N (*normal*) | B (*broken*) | C (*continued*)

(* A type for configurations *)
type configuration = (store * com * status) (*check if this makes sense; I might only 
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
  | s, Skip, N -> s
  | s, Assign (var_name, a), N -> (var_name, (evala s a))::s



  | s, Seq (Break, c2), N -> evalc (s, c2, B)
  | s, Seq (Continue, c2), N -> evalc (s, c2, C)
  | s, Seq (c1, c2), N -> let s' = evalc (s, c1, N) in evalc (s', c2, N)
  | s, Seq (While (b,c), c2), S -> 
    let s' = evalc (s, (While b, c) c1) in evalc (s', c2)
  | s, Seq (While _, c2), D -> evalc (s, c2)
  | s, Seq (_, c2), stat -> (s, c2, stat)



  | s, If (b, c_then, c_else), N -> 
    if evalb s b then evalc (s, c_then) else evalc (s, c_else)
  | s, While (b, c), D -> s
  | s, While (b, c), _ -> evalc (s, If (b, Seq (c, While (b,c)), Skip), N) 
  | s, (Print a), N -> print_endline (soi (evala s a)); s (*double check*)
  | s, (Test (i,b)) -> 
    if evalb s b then s else 
      let (l1, c1), (l2, c2) = i in
      let str = "TestFailed (test located between "
              ^ "line " ^ (soi l1) ^ ", character " ^ (soi c1) ^ " and "
              ^ "line " ^ (soi l2) ^ ", character " ^ (soi c2) 
      in print_endline str; raise (TestFailure str)
  | s, Break -> s
  | s, Continue -> raise (Continued s) 
