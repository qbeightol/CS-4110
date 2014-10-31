<<<<<<< HEAD

open Ast
open Fvs

let rec cps (e:exp) (k:cps_atom) : cps_exp =
  match e with
  | Var x -> CApp (CAtom k, CVar x)
  | App (e1, e2) -> cps e1 (app_new_k e2 k)
  | Lam (x, e) ->
      let k' = fresh "k'" (fvs_exp e) in
      CApp (CAtom k, CLam (x, CAtom (CLam (k', cps e (CVar k')))))
  | Let (x, e1, e2) -> cps (App (Lam (x, e2), e1)) k
  | Unit -> CApp (CAtom k, CUnit)
  | Int n -> CApp (CAtom k, CInt n)
  | Plus (e1, e2) -> cps e1 (plus_new_k e2 k)
  | Pair (e1, e2) -> cps e2 (pair_new_k e2 k)
  | Fst p -> cps p (fst_new_k k)
  | Snd p -> cps p (snd_new_k k)
  | True -> CApp (CAtom k, CTrue)
  | False -> CApp (CAtom k, CFalse)
  | Eq (e1, e2) -> cps e1 (eq_new_k e2 k)
  | If (e1, e2, e3) -> cps e1 (if_new_k e2 e3 k)

and plus_new_k (e2: exp) (k: cps_atom) : cps_atom =
  let n = fresh "n" (fvs_exp e2) in
  CLam (n, cps e2 (CLam ("m", CApp (CAtom k, CPlus (n, "m")))))

and pair_new_k (e2: exp) (k: cps_atom) : cps_atom =
  let v = fresh "v" (fvs_exp e2) in
  CLam (v, cps e2 (CLam ("w", CApp (CAtom k, CPair(v,"w")))))

and fst_new_k (k: cps_atom) : cps_atom =
  CLam ("v", CApp (CAtom k, CFst "v"))

and snd_new_k (k:cps_atom) : cps_atom =
  CLam ("v", CApp (CAtom k, CSnd "v"))

and app_new_k (e2:exp) (k:cps_atom) : cps_atom =
  let f = fresh "f" (fvs_exp e2) in
  CLam (f, cps e2 (CLam ("v", CApp ((CApp (CAtom (CVar f), CVar "v"), k)))))

and eq_new_k (e2:exp) (k:cps_atom) : cps_atom =
  let v1 = fresh "v1" (fvs_exp e2) in
  let temp = CApp (CAtom k, CEq (v1, "v2")) in
  CLam (v1, cps e2 (CLam ("v2", temp)))

and if_new_k (e2:exp) (e3:exp) (k:cps_atom) : cps_atom =
  let b = fresh "b" (fvs_exp e2) in
  let v2 = fresh "v2" (fvs_exp e3) in
  let temp = CLam ("v3", CApp (CAtom k, CIf (b, v2, "v3"))) in
  CLam (b, cps e2 (CLam (v2, cps e3 temp)))
=======
open Ast
open Fvs

let rec cps (e:exp) (k:cps_atom) : cps_exp = 
  failwith "John C. Reynolds"
>>>>>>> 64474e6ad04efbfa8ddc9ef574c13cb0fc6c78f0
