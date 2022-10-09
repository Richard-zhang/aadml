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

let eval_nullary env exp =
  match exp with
  | Zero -> 0.0
  | One -> 1.0
  | Const a -> a
  | Var id -> lookup id env
  | _ -> failwith "nullary operator only"

let eval_unary exp =
  match exp with
  | Sin _ -> Float.sin
  | Cos _ -> Float.cos
  | Ln _ -> Float.log
  | E _ -> Float.exp
  | _ -> failwith "unary operator only"

let eval_binary exp =
  match exp with
  | Add _ -> ( +. )
  | Sub _ -> ( -. )
  | Div _ -> ( /. )
  | Mul _ -> ( *. )
  | _ -> failwith "binary operator only"

let diff_nullary id = function
  | Var x -> if id = x then one else zero
  | Zero -> zero
  | One -> zero
  | Const _ -> zero
  | _ -> failwith "nullary operator only"

let diff_unary exp =
  let diff_unary_help = function
    | Sin a -> cos a
    | Cos a -> sub zero (sin a)
    | E a -> e a
    | Ln a -> div one a
    | _ -> failwith "unary operator only"
  in
  mul (diff_unary_help exp)

let diff_binary x a_dot b_dot =
  match x with
  | Add _ -> add a_dot b_dot
  | Sub _ -> sub a_dot b_dot
  | Mul (a, b) -> add (mul a_dot b) (mul a b_dot)
  | Div (a, b) ->
      let u'v = mul a_dot b in
      let uv' = mul a b_dot in
      div (sub u'v uv') (mul b b)
  | _ -> failwith "not binary operator"

let eval env x = fold_cps eval_binary eval_unary (eval_nullary env) x Base.Fn.id
let diff id x = fold_cps diff_binary diff_unary (diff_nullary id) x Base.Fn.id

let symbolic_diff env id formula =
  let diff_formula = diff id formula in
  eval env diff_formula

[@@@warning "-32"]

let eval_diff_nullary id = function
  | Var x -> if id = x then 1.0 else 0.0
  | Zero -> 0.0
  | One -> 0.0
  | Const _ -> 0.0
  | _ -> failwith "nullary"

(*
  Base.Fn.compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'b)
  ( *. ) : float1 -> (float2 -> float3)
  evaluator : float0 -> float1
  compose : (float1 -> (float2 -> float3)) -> (float0 -> float1) -> (float 0 -> (float 2 -> float 3))
  compose ( *. ) evaluator : float0 -> float2 -> float3
*)
let eval_diff_unary op =
  Base.Fn.compose ( *. )
    (match op with
    | Sin _ -> Float.cos
    | Cos _ -> Base.Fn.compose Float.neg Float.cos
    | E _ -> Float.exp
    | Ln _ -> ( /. ) 1.0
    | _ -> failwith "unary")

let eval_diff_binary op (left_val, left_diff) (right_val, right_diff) =
  match op with
  | Add _ -> left_diff +. right_diff
  | Sub _ -> left_diff -. right_diff
  | Mul _ -> (left_diff *. right_val) +. (left_val *. right_diff)
  | Div _ ->
      let u'v = left_diff *. right_val in
      let uv' = left_val *. right_diff in
      (u'v -. uv') /. (right_val *. right_val)
  | _ -> failwith "binary"

type forward_t = float * float

let combine_eval_diff_nullary env id x : forward_t =
  (eval_nullary env x, eval_diff_nullary id x)

let combine_eval_diff_unary x ((prev_value, prev_diff) : forward_t) : forward_t
    =
  (eval_unary x prev_value, eval_diff_unary x prev_value prev_diff)

let combine_eval_diff_binary x (left : forward_t) (right : forward_t) :
    forward_t =
  (eval_binary x (fst left) (fst right), eval_diff_binary x left right)

(*
  forword diff calculate forward and diff at the same time 
*)
let forward_diff env id x =
  fold_cps combine_eval_diff_binary combine_eval_diff_unary
    (combine_eval_diff_nullary env id)
    x snd

let%test_unit "y = x_0 + (x_0 * x_1), x_0 = 2, x_1 = 3" =
  let formula = add (var 0) (mul (var 0) (var 1)) in
  let env = empty |> update 0 2.0 |> update 1 3.0 in
  [%test_eq: Base.float] (forward_diff env 0 formula) 4.0

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
  let _ = diff 0 formula in
  [%test_eq: Base.float] 0.0 0.0

let%test_unit "const" = [%test_eq: Base.float] (eval empty (Const 1.0)) 1.0
let%test "const" = eval empty (Const 1.0) = 1.0

let test_simple diff_evals formula a derv_a b derv_b =
  let fuzzy_comp a b =
    [%test_pred: Base.float] (Base.Fn.flip ( < ) 0.000001) (abs_float (a -. b))
  in
  let env = empty |> update 0 a |> update 1 b in
  let _ =
    List.map
      (fun diff_eval -> fuzzy_comp (diff_eval env 0 formula) derv_a)
      diff_evals
  in
  let _ =
    List.map
      (fun diff_eval -> fuzzy_comp (diff_eval env 1 formula) derv_b)
      diff_evals
  in
  ()

let%test_unit "smoke test" = test_can_derv (var 0)
let%test_unit "y = sin(x_0)" = test_can_derv (sin (var 0))

let diff_evaluators = [ symbolic_diff; forward_diff ]

let%test_unit "y=x_0" =
  let formula = var 0 in
  test_simple diff_evaluators formula 3.0 1.0 1.0 0.0

let%test_unit "y = x_0 + x_1" =
  let formula = add (var 0) (var 1) in
  test_simple diff_evaluators formula 3.0 1.0 1.0 1.0

let%test_unit "y = x_0 * x_1" =
  let formula = mul (var 0) (var 1) in
  test_simple diff_evaluators formula 3.0 4.0 4.0 3.0

let%test_unit "complex formula" =
  test_simple diff_evaluators (test_formula ()) 1.0 (-0.181974) 1.0 (-0.118142)

let rec fact n cont =
  if n = 0 then cont 1
  else (fact [@tailcall]) (n - 1) (fun fact_n_1 -> fact_n_1 * n |> cont)

let%test "fact cps style" = fact 4 Base.Fn.id = 24
