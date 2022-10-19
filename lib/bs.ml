(*
1. understand black sholes equation
2. take a look at nag AAD tutorial
3. BS in aad
4. test case for delta using analytical solution
*)
open Expr

let bs ~vol ~stock ~strike ~t ~rate =
  let vol = var vol in
  let stock = var stock in
  let strike = var strike in
  let expiry_time = var t in
  let rate = var rate in
  let vol_square = mul vol vol in
  let two = const 2.0 in
  let discount_factor = e (neg (mul rate expiry_time)) in
  let vol_sqrt_t = mul vol (sqrt expiry_time) in
  let d_1 =
    div
      (add
         (ln (div stock strike))
         (mul expiry_time (add rate (div vol_square two))))
      vol_sqrt_t
  in
  let d_2 = sub d_1 vol_sqrt_t in
  let discounted_stock = mul stock (Normal.cdf d_1) in
  let discounted_option = mul (mul strike discount_factor) (Normal.cdf d_2) in
  sub discounted_stock discounted_option

let eval_bs_fomula ~(vol : float) ~(stock : float) ~(strike : float)
    ~(t : float) ~(rate : float) =
  let vol_i = 0 in
  let stock_i = 1 in
  let strike_i = 2 in
  let t_i = 3 in
  let rate_i = 4 in
  let env =
    empty |> update vol_i vol |> update stock_i stock |> update strike_i strike
    |> update t_i t |> update rate_i rate
  in
  (env, bs ~vol:vol_i ~stock:stock_i ~strike:strike_i ~t:t_i ~rate:rate_i)

let eval_bs ~(vol : float) ~(stock : float) ~(strike : float) ~(t : float)
    ~(rate : float) =
  let env, bs_formula = eval_bs_fomula ~vol ~stock ~strike ~t ~rate in
  eval env bs_formula

let%test_unit "bs call valuation 1" =
  let value = eval_bs ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.001 value 58.82

let%test_unit "bs call valuation 2" =
  let value = eval_bs ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 91.98

let%test_unit "bs call valuation 3" =
  let value = eval_bs ~vol:0.15 ~stock:250.0 ~strike:300.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 29.89

let%test_unit "bs call valuation 4" =
  let value = eval_bs ~vol:0.40 ~stock:250.0 ~strike:300.0 ~t:5.0 ~rate:0.03 in
  Util.fuzzy_compare ~accuracy:0.01 value 83.69

let%test_unit "bs greek 1" =
  let env, formula =
    eval_bs_fomula ~vol:0.15 ~stock:300.0 ~strike:250.0 ~t:1.0 ~rate:0.03
  in
  let all_diff = Diff.backward_all_diff env formula in
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 1) 0.932;
  Util.fuzzy_compare ~accuracy:0.01 (all_diff |> Expr.lookup 3) (-9.579);
  ()
