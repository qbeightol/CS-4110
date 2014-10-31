open Ast


exception IllformedExpression

(******************************************************************************)
(* Helper Functions                                                           *)
(******************************************************************************)

(*takes in a cps value, and returns an int, if the value is an int*)
let pop_int = function
  | VInt n -> n
  | _ -> failwith "not an int"

let fst = function
  | VPair (x, _) -> x
  | _ -> failwith "not a pair"

let snd = function
  | VPair (_, y) -> y
  | _ -> failwith "not a pair"

let if' g x y z =
  match (g x) with
  (*I'm including the environment here in order to lazily evaluate x y *)
  | VTrue -> g y
  | VFalse -> g z
  | _ -> failwith "not a bool"

(*adds all the bindings in env1 to env2, overwriting the old bindings in env2,
if present*)
let merge_envs env1 env2 x =
  try lookup env1 x
  with UnboundVariable _ -> lookup env2 x



(******************************************************************************)
(* Evaluation Functions                                                       *)
(******************************************************************************)

(* evaluate CPS expression e in environment g *)
let rec eval (g:cps_env) (c:cps_exp) : cps_val =
  match c with
  | CAtom a -> eval_atom g a
  | CApp (e, a) ->
      match eval g e with
      | VClosure (env, x, body) ->
          let g_and_env = merge_envs env g in
          let new_env = extend g_and_env x (eval g (CAtom a)) in
          eval new_env body
      | _ -> failwith "what?"

(* evaluate CPS atom a in environment g *)
and eval_atom (g:cps_env) (a:cps_atom) : cps_val =
  match a with
  | CVar x -> g x
  | CLam (x, e) -> VClosure (g, x, e)
  | CUnit -> VUnit
  | CInt n -> VInt n
  | CPlus (x, y) -> VInt (pop_int (g x) + pop_int (g y))
  | CPair (x, y) -> VPair (g x, g y)
  | CFst p -> fst (g p)
  | CSnd p -> snd (g p)
  | CTrue -> VTrue
  | CFalse -> VFalse
  | CEq (x, y) -> if g x == g y then VTrue else VFalse
    (*we may want to disallow comparing closures...*)
  | CIf (x, y, z) -> if' g x y z
