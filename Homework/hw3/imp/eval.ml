open Ast

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var
    
(* A type for stores *)
type store = (var, int) Hashtbl.t

(*a wrapper type capable of holding the three kinds of expressions in IMP*)
type exp = Aexp of aexp | Bexp of bexp | Com of com

(* A type for configurations *)
type configuration = (store * exp) (*check if this makes sense; I might only 
need to focus on commands*)

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
  (Hashtbl.create 10, Com c)
  failwith "Not yet implemented"

(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
  failwith "Not yet implemented"
