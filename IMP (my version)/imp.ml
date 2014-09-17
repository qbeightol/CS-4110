
type var = string

type exp =
    Int of int
  | Var of var
  | Plus of exp * exp
  | Mult of exp * exp
  | Assgn of var * exp * exp

let rec show (e: exp) : string = 
  match e with
  | Int n -> string_of_int n
  | Var x -> x
  | Plus (e1,e2) -> "(" ^ (show e1) ^ " + " ^ (show e2) ^ ")"
  | Mult (e1, e2) -> "(" ^ (show e1) ^ " * " ^ (show e2) ^ ")"
  | Assgn (x, e1, e2) -> "(" ^ x ^ ":= " ^ (show e1) ^ "; " ^ (show e2) ^ ")"


type store = string -> int

let empty_store = fun x -> failwith "not bound"

let add_binding (s: store) (var_name: string) (value: int) = 
  fun v -> if v = var_name then value else s v



