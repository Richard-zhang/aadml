open Expr

let pdf x =
  let coef = const (1.0 /. Float.sqrt (Float.pi *. 2.0)) in
  let term = e (div (mul x x) (const (-2.0))) in
  mul coef term

(**
  1. using the normal approximation
*)
let cdf x =
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
  let t_5 = mul t_3 t_2 in
  let pdf_coef = pdf x in
  let term =
    add (mul b_1 t)
      (add (mul b_2 t_2) (add (mul b_3 t_3) (add (mul b_4 t_4) (mul b_5 t_5))))
  in
  sub one (mul pdf_coef term)

let pure_cdf x = 0.5 *. (1.0 +. Float.erf (x /. Float.sqrt 2.0))

(* this is the correct handling of negative *)
let rec eval_cdf x =
  if x <= 0. then 1. -. eval_cdf (Float.neg x)
  else
    let id = 0 in
    let env = empty |> update id x in
    let cdf_formula = cdf (var id) in
    Expr.eval env cdf_formula

let%test_unit "cdf(0.4)" = Util.fuzzy_compare (pure_cdf 0.4) 0.65542

let%test_unit "F(z <= 0.4) == 0.65542" =
  Util.fuzzy_compare (eval_cdf 0.4) 0.65542

let%test_unit "cdf(-1.)" = Util.fuzzy_compare (pure_cdf (-1.)) 0.15865525
let%test_unit "F(z <= -1.)" = Util.fuzzy_compare (1. -. eval_cdf 1.) 0.15865525

let%test_unit "test_case" =
  let _ =
    List.map
      (fun x -> Util.fuzzy_compare (eval_cdf x) (pure_cdf x))
      [
        -3.; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 0.95; 1.5; 4.; 8.; 9.;
      ]
  in
  ()
