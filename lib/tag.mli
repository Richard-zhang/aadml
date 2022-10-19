type _ tag_expr =
  | Const : 'tag * 'a -> ('tag * 'a) tag_expr
  | Mul :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Add :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Sub :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Div :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Sin : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Cos : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Ln : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | E : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Sqrt : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Zero : 'tag -> ('tag * 'a) tag_expr
  | One : 'tag -> ('tag * 'a) tag_expr
  | Var : 'tag * int -> ('tag * 'a) tag_expr

val get_tag : ('a * 'b) tag_expr -> 'a

val add_tag :
  'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr

val mul_tag :
  'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr

val sub_tag :
  'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr

val div_tag :
  'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr

val cos_tag : 'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr
val sin_tag : 'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr
val e_tag : 'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr
val ln_tag : 'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr
val sqrt_tag : 'a -> ('a * 'b) tag_expr -> ('a * 'b) tag_expr
val zero_tag : 'a -> ('a * 'b) tag_expr
val one_tag : 'a -> ('a * 'b) tag_expr
val var_tag : 'a -> int -> ('a * 'b) tag_expr
val const_tag : 'a -> 'b -> ('a * 'b) tag_expr

val fold_cps_tag :
  (('a * 'b) tag_expr -> 'c -> 'c -> 'c) ->
  (('a * 'b) tag_expr -> 'c -> 'c) ->
  (('a * 'b) tag_expr -> 'c) ->
  ('a * 'b) tag_expr ->
  ('c -> 'd) ->
  'd
