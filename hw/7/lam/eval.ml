open Ast

exception IllformedExpression 

(* evaluate CPS expression e in environment g *)
let rec eval (g:cps_env) (c:cps_exp) : cps_val = 
  match c with
  | CApp (exp, atom) -> failwith "hello"
  | CAtom atom -> failwith "bonjour"

(* evaluate CPS atom a in environment g *) 
and eval_atom (g:cps_env) (a:cps_atom) : cps_val = 
  match a with
  | CVar var -> failwith "hola"
  | CLam (var, exp) -> failwith "salaam"
  | CUnit -> failwith "namaste"
  | CInt i -> failwith "shalom"
  | CPlus (var1, var2) -> failwith "guten tag" 
  | CPair (var1, var2) -> failwith "aloha" 
  | CFst var -> failwith "ciao"
  | CSnd var -> failwith "jambo"
  | CTrue -> failwith "hei"
  | CFalse -> failwith "goddag"
  | CEq (var1, var2) -> failwith "hallo"
  | CIf (b, branch_true, branch_false) -> failwith "ola"

