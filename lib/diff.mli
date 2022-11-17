val diff : int -> float Expr.expr -> float Expr.expr
(** symbolic differentation *)

val symbolic_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate symbolic differentation *)

val forward_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate feed forward differentation *)

val backward_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate backward differentation *)

val backward_all_diff : float Expr.env -> float Expr.expr -> float Expr.env
(** evaluate differentation with regards to all input variable *)

val debug_backward_feedforwrad :
  float Expr.env -> (unit, 'a) Expr.tag_expr -> (float, float) Expr.tag_expr

val debug_backward_backprop :
  float Expr.env -> (unit, 'a) Expr.tag_expr -> (float, float) Expr.tag_expr
