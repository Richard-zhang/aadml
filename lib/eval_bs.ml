open Expr

(*
val eval :
  vol:float -> stock:float -> strike:float -> t:float -> rate:float -> float

val eval_formula :
  vol:float -> stock:float -> strike:float -> t:float -> rate:float -> (float Expr.env * float Expr.expr)
*)
let eval_formula ~(vol : float) ~(stock : float) ~(strike : float) ~(t : float)
    ~(rate : float) =
  let vol_i = 0 in
  let stock_i = 1 in
  let strike_i = 2 in
  let t_i = 3 in
  let rate_i = 4 in
  let env =
    empty |> update vol_i vol |> update stock_i stock |> update strike_i strike
    |> update t_i t |> update rate_i rate
  in
  ( env,
    Bs.formula ~vol:vol_i ~stock:stock_i ~strike:strike_i ~t:t_i ~rate:rate_i
  )

let eval ~(vol : float) ~(stock : float) ~(strike : float) ~(t : float)
    ~(rate : float) =
  let env, bs_formula = eval_formula ~vol ~stock ~strike ~t ~rate in
  eval env bs_formula

let%test_unit "bs call valuation 1" =
  let value = eval ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 58.82

let%test_unit "bs call valuation 2" =
  let value = eval ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 91.98

let%test_unit "bs call valuation 3" =
  let value = eval ~vol:0.15 ~stock:250.0 ~strike:300.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 29.89

let%test_unit "bs call valuation 4" =
  let value = eval ~vol:0.40 ~stock:250.0 ~strike:300.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 83.69

let%test_unit "bs greek 1" =
  let env, formula =
    eval_formula ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03
  in
  let all_diff = Diff.backward_all_diff env formula in
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 1) 0.932;
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 3) 9.579;
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 0) 39.413;
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 4) 220.765;
  ()

let%test_unit "bs greek 4" =
  let env, formula =
    eval_formula ~vol:0.60 ~stock:100.0 ~strike:500.0 ~t:3.0 ~rate:0.04
  in
  let all_diff = Diff.backward_all_diff env formula in
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 1) 0.180;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 3) 5.003;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 0) 45.523;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 4) 33.818;
  ()

let%test_unit "bs greek 5" =
  let env, formula =
    eval_formula ~vol:0.50 ~stock:500.0 ~strike:100.0 ~t:10.0 ~rate:0.02
  in
  let all_diff = Diff.backward_all_diff env formula in
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 1) 0.974;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 3) 3.471;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 0) 97.021;
  Util.fuzzy_compare ~accuracy:0.001 (all_diff |> Expr.lookup 4) 522.565;
  ()

let%test_unit "bs greek 2" =
  let env, formula =
    eval_formula ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03
  in
  Util.fuzzy_compare ~accuracy:0.001 (Diff.forward_diff env 1 formula) 0.932;
  Util.fuzzy_compare ~accuracy:0.001 (Diff.forward_diff env 3 formula) 9.579;
  Util.fuzzy_compare ~accuracy:0.001 (Diff.forward_diff env 0 formula) 39.413;
  ()

let%test_unit "bs greek 3" =
  let env, formula =
    eval_formula ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03
  in
  Util.fuzzy_compare ~accuracy:0.001 (Diff.symbolic_diff env 1 formula) 0.932;
  Util.fuzzy_compare ~accuracy:0.001 (Diff.symbolic_diff env 3 formula) 9.579;
  Util.fuzzy_compare ~accuracy:0.001 (Diff.symbolic_diff env 0 formula) 39.413;
  ()
