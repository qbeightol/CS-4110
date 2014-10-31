open Ast

exception IllformedExpression 

(* evaluate CPS expression e in environment g *)
let rec eval (g:cps_env) (c:cps_exp) : cps_val = 
  match c with
  | CApp (exp, atom) -> failwith "hello"
  | CAtom atom -> failwith "bonjour"

(* evaluate CPS atom a in environment g *) 
and eval_atom (g:cps_env) (a:cps_atom) : cps_val = 
  failwith "F. Lockwood Morris"
