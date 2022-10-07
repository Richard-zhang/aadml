module IntMap = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

type 'a env = 'a IntMap.t

let empty = IntMap.empty
let update = IntMap.add
let lookup key = IntMap.find key

type _ expr =
  | Const : 'a -> 'a expr
  | Mul : 'a expr * 'a expr -> 'a expr
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Div : 'a expr * 'a expr -> 'a expr
  | Sin : 'a expr -> 'a expr
  | Cos : 'a expr -> 'a expr
  | Ln : 'a expr -> 'a expr
  | E : 'a expr -> 'a expr
  | Zero : 'a expr
  | One : 'a expr
  | Var : int -> 'a expr

let rec fold_cps bin_op unary_op nullary_op x cont =
  let unary_apply a exp =
    (fold_cps [@tailcall]) bin_op unary_op nullary_op a
      (Base.Fn.compose cont (unary_op exp))
  in
  let binary_apply a b exp =
    (fold_cps bin_op unary_op nullary_op [@tailcall]) a (fun r_a ->
        (fold_cps bin_op unary_op nullary_op [@tailcall]) b (fun r_b ->
            (bin_op exp) r_a r_b |> cont))
  in
  match x with
  | Mul (a, b) -> binary_apply a b x
  | Add (a, b) -> binary_apply a b x
  | Sub (a, b) -> binary_apply a b x
  | Div (a, b) -> binary_apply a b x
  | Sin a -> unary_apply a x
  | Cos a -> unary_apply a x
  | Ln a -> unary_apply a x
  | E a -> unary_apply a x
  | Zero -> x |> nullary_op |> cont
  | One -> x |> nullary_op |> cont
  | Const _ -> x |> nullary_op |> cont
  | Var _ -> x |> nullary_op |> cont

let eval_unary_helper exp =
  match exp with
  | Sin _ -> Float.sin
  | Cos _ -> Float.cos
  | Ln _ -> Float.log
  | E _ -> Float.exp
  | _ -> failwith "unary operator only"

let eval_binary_helper exp =
  match exp with
  | Add _ -> ( +. )
  | Sub _ -> ( -. )
  | Div _ -> ( /. )
  | Mul _ -> ( *. )
  | _ -> failwith "binary operator only"

let eval_nullary_helper env exp =
  match exp with
  | Zero -> 0.0
  | One -> 1.0
  | Const a -> a
  | Var id -> lookup id env
  | _ -> failwith "nullary operator only"

let eval x env =
  fold_cps eval_binary_helper eval_unary_helper (eval_nullary_helper env) x
    Base.Fn.id

let add a b = Add (a, b)
let mul a b = Mul (a, b)
let sub a b = Sub (a, b)
let div a b = Div (a, b)
let cos a = Cos a
let sin a = Sin a
let e a = E a
let ln a = Ln a
let zero = Zero
let one = One
let var id = Var id

let diff_helper = function
  | Zero -> zero
  | One -> zero
  | Const _ -> zero
  | Var _ -> one
  | Sin a -> cos a
  | Cos a -> sub zero (sin a)
  | E a -> e a
  | Ln a -> div one a
  | _ -> failwith "binary operator"

let diff_binary_helper a_dot b_dot = function
  | Add _ -> add a_dot b_dot
  | Sub _ -> sub a_dot b_dot
  | Mul (a, b) -> add (mul a_dot b) (mul a b_dot)
  | Div (a, b) ->
      let u'v = mul a_dot b in
      let uv' = mul a b_dot in
      div (sub u'v uv') (mul b b)
  | _ -> failwith "not binary operator"

let rec diff_cps x id cont =
  let apply_chain_rule a exp =
    (diff_cps [@tailcall]) a id (Base.Fn.compose cont (mul (diff_helper exp)))
  in
  let apply_binary_rule a b exp =
    (diff_cps [@tailcall]) b id (fun b_dot ->
        (diff_cps [@tailcall]) a id (fun a_dot ->
            diff_binary_helper a_dot b_dot exp |> cont))
  in
  match x with
  | Var x -> cont (if id = x then one else zero)
  | Zero -> cont zero
  | One -> cont zero
  | Const _ -> cont zero
  | Add (a, b) as exp -> apply_binary_rule a b exp
  | Mul (a, b) as exp -> apply_binary_rule a b exp
  | Sub (a, b) as exp -> apply_binary_rule a b exp
  | Div (a, b) as exp -> apply_binary_rule a b exp
  | Sin a as exp -> apply_chain_rule a exp
  | Cos a as exp -> apply_chain_rule a exp
  | E a as exp -> apply_chain_rule a exp
  | Ln a as exp -> apply_chain_rule a exp

(* symbolic diff *)
let diff x id = diff_cps x id Base.Fn.id

(*
forword diff
*)

(* type res = { value : float; diff : float } *)

(* could this be a specialization of eval_cps *)
(* could diff a specialization of eval_cps *)
(*
let forward_diff formula env id : float =
  let rec formula_diff_impl formula id : res =
    match formula with
    | Var x -> {value = lookup id env; diff =}

  in
  let res = formula_diff_impl formula env id in
  res.diff
*)

let test_formula () =
  let x_0 = var 0 in
  let x_1 = var 1 in
  let x_01 = mul x_0 x_1 in
  let sin_x_0 = sin x_0 in
  let add_x01_sin = add x_01 sin_x_0 in
  let e = e add_x01_sin in
  let add_1_e = add one e in
  div one add_1_e

let test_can_derv formula =
  let _ = diff formula 0 in
  [%test_eq: Base.float] 0.0 0.0

let%test_unit "const" = [%test_eq: Base.float] (eval (Const 1.0) empty) 1.0
let%test "const" = eval (Const 1.0) empty = 1.0

let test_simple formula a derv_a b derv_b =
  let fuzzy_comp a b =
    [%test_pred: Base.float] (fun a -> a < 0.000001) (abs_float (a -. b))
  in
  let env = empty |> update 0 a |> update 1 b in
  let diff_x0 = diff formula 0 in
  let diff_x1 = diff formula 1 in
  let diff_x0 = eval diff_x0 env in
  let diff_x1 = eval diff_x1 env in
  fuzzy_comp diff_x0 derv_a;
  fuzzy_comp diff_x1 derv_b

let%test_unit "smoke test" = test_can_derv (var 0)
let%test_unit "y = sin(x_0)" = test_can_derv (sin (var 0))

let%test_unit "y=x_0" =
  let formula = var 0 in
  test_simple formula 3.0 1.0 1.0 0.0

let%test_unit "y = x_0 + x_1" =
  let formula = add (var 0) (var 1) in
  test_simple formula 3.0 1.0 1.0 1.0

let%test_unit "y = x_0 * x_1" =
  let formula = mul (var 0) (var 1) in
  test_simple formula 3.0 4.0 4.0 3.0

let%test_unit "complex formula" =
  test_simple (test_formula ()) 1.0 (-0.181974) 1.0 (-0.118142)

let rec fact n cont =
  if n = 0 then cont 1
  else (fact [@tailcall]) (n - 1) (fun fact_n_1 -> fact_n_1 * n |> cont)

let%test "fact cps style" = fact 4 Base.Fn.id = 24
