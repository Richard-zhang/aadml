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
  | Sqrt : 'a expr -> 'a expr
  | Zero : 'a expr
  | One : 'a expr
  | Var : int -> 'a expr
  | Max : 'a expr * 'a expr -> 'a expr
  | Min : 'a expr * 'a expr -> 'a expr
  | Not : bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Equal : 'a expr * 'a expr -> bool expr
  | Less : 'a expr * 'a expr -> bool expr
  | IfThenElse : bool expr * 'a expr * 'a expr -> 'a expr

val fold_cps :
  ('a expr -> 'b -> 'b -> 'b) ->
  ('a expr -> 'b -> 'b) ->
  ('a expr -> 'b) ->
  'a expr ->
  ('b -> 'c) ->
  'c

val add : 'a expr -> 'a expr -> 'a expr
val mul : 'a expr -> 'a expr -> 'a expr
val sub : 'a expr -> 'a expr -> 'a expr
val div : 'a expr -> 'a expr -> 'a expr
val cos : 'a expr -> 'a expr
val sin : 'a expr -> 'a expr
val e : 'a expr -> 'a expr
val ln : 'a expr -> 'a expr
val sqrt : 'a expr -> 'a expr
val zero : 'a expr
val one : 'a expr
val var : int -> 'a expr
val const : 'a -> 'a expr
val neg : 'a expr -> 'a expr
val power : int -> 'a expr -> 'a expr

val eval : float env -> float expr -> float
(** naive evaluation implemented via tree traversal *)

val string_of_op : show:('a -> string) -> 'a expr -> string
