val diff : int -> 'a Expr.expr -> 'a Expr.expr
(** symbolic differentation *)

val symbolic_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate symbolic differentation *)

val forward_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate feed forward differentation *)

val backward_diff : float Expr.env -> int -> float Expr.expr -> float
(** evaluate backward differentation *)
