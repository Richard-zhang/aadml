open Expr

let pdf id =
  let coef = div one (sqrt (const (2.0 *. Float.pi))) in
  let term = e (div (mul (var id) (var id)) (const (-2.0))) in
   mul coef term

(**
  1. using the normal approximation
*)
let cdf id =
  let x = var id in
  let b_0 = const 0.2316419 in
  let b_1 = const 0.319381530 in
  let b_2 = const (Float.neg 0.356563782) in
  let b_3 = const 1.781477937 in
  let b_4 = const (Float.neg 1.821255978) in
  let b_5 = const 1.330274429 in
  let t = div one (add one (mul b_0 x)) in
  let t_2 = mul t t in
  let t_3 = mul t_2 t in
  let t_4 = mul t_2 t_2 in
  let t_5 = mul t_2 t_3 in
  let pdf_coef = pdf id in
  let term =
    add (mul b_1 t)
      (add (mul b_2 t_2) (add (mul b_3 t_3) (add (mul b_4 t_4) (mul b_5 t_5))))
  in
  sub one (mul pdf_coef term)

let%test_unit "F(z <= 0.4) == 0.65542" =
  let id = 0 in
  let env = empty |> update id 0.4 in
  let cdf_formula = cdf id in
  Util.fuzzy_compare (Expr.eval env cdf_formula) 0.65542
