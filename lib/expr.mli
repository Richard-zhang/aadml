(*
  support operation
  sin(x)
  cos(x)
  e^(x)
  ln(x)
  +
  -
  *
  /
*)
module IntMap : Map.S with type key = int

type 'a env = 'a IntMap.t

val empty : 'a env
val update : int -> 'a -> 'a env -> 'a env
val lookup : int -> 'a env -> 'a

type _ expr =
  | Const : 'a -> 'a expr
  | Mul : 'a expr * 'a expr -> 'a expr
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Div : 'a expr * 'a expr -> 'a expr
  | Sin : 'a expr -> 'a expr
  | Cos : 'a expr -> 'a expr
  | Ln : 'a expr -> 'a expr
  | E : 'a expr -> 'a expr
  | Zero : 'a expr
  | One : 'a expr
  | Var : int -> 'a expr

val test_formula: unit -> 'a expr

val eval : float expr -> float env -> float
val add : 'a expr -> 'a expr -> 'a expr
val mul : 'a expr -> 'a expr -> 'a expr
val sub : 'a expr -> 'a expr -> 'a expr
val div : 'a expr -> 'a expr -> 'a expr
val cos : 'a expr -> 'a expr
val sin : 'a expr -> 'a expr
val e : 'a expr -> 'a expr
val ln : 'a expr -> 'a expr
val zero : 'a expr
val one : 'a expr
val var : int -> 'a expr
val diff : 'a expr -> int -> 'a expr

