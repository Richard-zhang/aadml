val bs :
  vol:int -> stock:int -> strike:int -> t:int -> rate:int -> float Expr.expr

val eval_bs :
  vol:float -> stock:float -> strike:float -> t:float -> rate:float -> float
