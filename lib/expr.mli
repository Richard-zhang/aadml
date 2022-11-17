module IntMap : Map.S with type key = int

type 'a env = 'a IntMap.t

val empty : 'a env
val update : int -> 'a -> 'a env -> 'a env
val lookup : int -> 'a env -> 'a

type ('tag, _) tag_expr =
  | Const : 'tag * float -> ('tag, float) tag_expr
  | Mul :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Add :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Sub :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Div :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Sin : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Cos : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Ln : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | E : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Sqrt : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Erf : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Var : 'tag * int -> ('tag, float) tag_expr
  | Max :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Min :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Not : 'tag * ('tag, bool) tag_expr -> ('tag, bool) tag_expr
  | And :
      'tag * ('tag, bool) tag_expr * ('tag, bool) tag_expr
      -> ('tag, bool) tag_expr
  | Or :
      'tag * ('tag, bool) tag_expr * ('tag, bool) tag_expr
      -> ('tag, bool) tag_expr
  | Equal :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, bool) tag_expr
  | Less :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, bool) tag_expr
  | Cond :
      'tag * ('tag, bool) tag_expr * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr

type ('tag, 'a) nullary = { nop : 'elt. ('tag, 'elt) tag_expr -> 'a }
type ('tag, 'a) unary = { uop : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a }
type ('tag, 'a) binary = { bop : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a -> 'a }

type ('tag, 'a) ternary = {
  top : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a -> 'a -> 'a;
}

val dummy_ternary : unit -> ('a, 'b) ternary
val get_tag : ('tag, 'b) tag_expr -> 'tag

val fold_cps :
  ('tag, 'b) ternary ->
  ('tag, 'b) binary ->
  ('tag, 'b) unary ->
  ('tag, 'b) nullary ->
  ('tag, 'a) tag_expr ->
  ('b -> 'c) ->
  'c

type 'a expr = (unit, 'a) tag_expr
type 'tag any = Any : ('tag, 'a) tag_expr -> 'tag any
type _ ty = TyFloat : float ty | TyBool : bool ty
type ('tag, 'a) cont = { run : 'elt. ('tag, 'elt) tag_expr -> 'a }

val spread : 'tag any -> ('tag, 'a) cont -> 'a
val tyExpr : ('tag, 'a) tag_expr -> 'a ty
val cast : ('tag, 'a) tag_expr -> 'b ty -> ('tag, 'b) tag_expr option
val magic_cast : ('tag, 'a) tag_expr -> 'b ty -> ('tag, 'b) tag_expr
val cast_to_float : ('tag, 'a) tag_expr -> ('tag, float) tag_expr
val cast_to_bool : ('tag, 'a) tag_expr -> ('tag, bool) tag_expr
val add : float expr -> float expr -> float expr
val mul : float expr -> float expr -> float expr
val sub : float expr -> float expr -> float expr
val div : float expr -> float expr -> float expr
val max : float expr -> float expr -> float expr
val min : float expr -> float expr -> float expr
val cos : float expr -> float expr
val sin : float expr -> float expr
val erf : float expr -> float expr
val e : float expr -> float expr
val ln : float expr -> float expr
val sqrt : float expr -> float expr
val zero : float expr
val one : float expr
val var : int -> float expr
val const : float -> float expr
val neg : float expr -> float expr
val power : int -> float expr -> float expr

val add_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val mul_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val sub_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val div_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val max_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val min_tag :
  'tag ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr ->
  ('tag, float) tag_expr

val cos_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val sin_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val e_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val ln_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val sqrt_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val erf_tag : 'tag -> ('tag, float) tag_expr -> ('tag, float) tag_expr
val zero_tag : 'tag -> ('tag, float) tag_expr
val one_tag : 'tag -> ('tag, float) tag_expr
val var_tag : 'tag -> int -> ('tag, float) tag_expr
val const_tag : 'tag -> float -> ('tag, float) tag_expr
val not_tag : 'tag -> ('tag, bool) tag_expr -> ('tag, bool) tag_expr

val eval : float env -> float expr -> float
(** naive evaluation implemented via tree traversal *)

val add_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val mul_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val sub_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val div_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val max_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val min_any_tag : 'tag -> 'tag any -> 'tag any -> 'tag any
val cos_any_tag : 'tag -> 'tag any -> 'tag any
val sin_any_tag : 'tag -> 'tag any -> 'tag any
val e_any_tag : 'tag -> 'tag any -> 'tag any
val ln_any_tag : 'tag -> 'tag any -> 'tag any
val sqrt_any_tag : 'tag -> 'tag any -> 'tag any
val erf_any_tag : 'tag -> 'tag any -> 'tag any
val zero_any_tag : 'tag -> 'tag any
val one_any_tag : 'tag -> 'tag any
val var_any_tag : 'tag -> int -> 'tag any
val const_any_tag : 'tag -> float -> 'tag any
val not_any_tag : 'tag -> 'tag any -> 'tag any

val string_of_op : show:('a -> string) -> ('tag, 'a) tag_expr -> string
(** pretty printing operator **)
