val graph : ?name_env:string Expr.env -> float Expr.expr -> string

val tag_graph :
  show:('tag -> string) ->
  ?name_env:string Expr.env ->
  ('tag, float) Expr.tag_expr ->
  string

val float_graph :
  ?name_env:string Expr.env -> (float, float) Expr.tag_expr -> string

val float_float_graph :
  ?name_env:string Expr.env -> (float * float, float) Expr.tag_expr -> string
