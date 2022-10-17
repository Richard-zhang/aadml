open Expr

let nullary_warning = "nullary operator only"
let unary_warning = "unary operator only"
let binary_warning = "binary operator only"

let diff_nullary id = function
  | Var x -> if id = x then one else zero
  | Zero -> zero
  | One -> zero
  | Const _ -> zero
  | _ -> failwith nullary_warning

let diff_unary exp =
  let diff_unary_help = function
    | Sin a -> cos a
    | Cos a -> sub zero (sin a)
    | E a -> e a
    | Ln a -> div one a
    | Sqrt a -> div one (mul (add one one) (sqrt a))
    | _ -> failwith unary_warning
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
  | _ -> failwith binary_warning

let diff id x = fold_cps diff_binary diff_unary (diff_nullary id) x Base.Fn.id

let symbolic_diff env id formula =
  let diff_formula = diff id formula in
  eval env diff_formula

[@@@warning "-32-34-37"]

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
    | Sqrt _ -> fun x -> x |> Float.sqrt |> ( /. ) 0.5
    | _ -> failwith unary_warning)

let eval_diff_binary op (left_val, left_diff) (right_val, right_diff) =
  match op with
  | Add _ -> left_diff +. right_diff
  | Sub _ -> left_diff -. right_diff
  | Mul _ -> (left_diff *. right_val) +. (left_val *. right_diff)
  | Div _ ->
      let u'v = left_diff *. right_val in
      let uv' = left_val *. right_diff in
      (u'v -. uv') /. (right_val *. right_val)
  | _ -> failwith binary_warning

type forward_t = float * float

let eval_nullary env exp =
  match exp with
  | Zero -> 0.0
  | One -> 1.0
  | Const a -> a
  | Var id -> lookup id env
  | _ -> failwith nullary_warning

let eval_unary exp =
  match exp with
  | Sin _ -> Float.sin
  | Cos _ -> Float.cos
  | Ln _ -> Float.log
  | E _ -> Float.exp
  | Sqrt _ -> Float.sqrt
  | _ -> failwith unary_warning

let eval_binary exp =
  match exp with
  | Add _ -> ( +. )
  | Sub _ -> ( -. )
  | Div _ -> ( /. )
  | Mul _ -> ( *. )
  | _ -> failwith binary_warning

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

(*
backward diff is two pass
1. forward pass: annotate the value at each operation
2. back propagation: traverse the graph backwards
*)

(* is there a way work around with this problem *)
type _ tag_expr =
  | Const : 'tag * 'a -> ('tag * 'a) tag_expr
  | Mul :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Add :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Sub :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Div :
      'tag * ('tag * 'a) tag_expr * ('tag * 'a) tag_expr
      -> ('tag * 'a) tag_expr
  | Sin : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Cos : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Ln : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | E : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Sqrt : 'tag * ('tag * 'a) tag_expr -> ('tag * 'a) tag_expr
  | Zero : 'tag -> ('tag * 'a) tag_expr
  | One : 'tag -> ('tag * 'a) tag_expr
  | Var : 'tag * int -> ('tag * 'a) tag_expr

let get_tag = function
  | Const (tag, _) -> tag
  | Mul (tag, _, _) -> tag
  | Add (tag, _, _) -> tag
  | Sub (tag, _, _) -> tag
  | Div (tag, _, _) -> tag
  | Sin (tag, _) -> tag
  | Cos (tag, _) -> tag
  | Ln (tag, _) -> tag
  | E (tag, _) -> tag
  | Sqrt (tag, _) -> tag
  | Zero tag -> tag
  | One tag -> tag
  | Var (tag, _) -> tag

let add_tag tag a b = Add (tag, a, b)
let mul_tag tag a b = Mul (tag, a, b)
let sub_tag tag a b = Sub (tag, a, b)
let div_tag tag a b = Div (tag, a, b)
let cos_tag tag a = Cos (tag, a)
let sin_tag tag a = Sin (tag, a)
let e_tag tag a = E (tag, a)
let ln_tag tag a = Ln (tag, a)
let sqrt_tag tag v = Sqrt (tag, v)
let zero_tag tag = Zero tag
let one_tag tag = One tag
let var_tag tag id = Var (tag, id)
let const_tag tag v = Const (tag, v)

let rec fold_cps_tag bin_op unary_op nullary_op x cont =
  let unary_apply a exp =
    (fold_cps_tag [@tailcall]) bin_op unary_op nullary_op a
      (Base.Fn.compose cont (unary_op exp))
  in
  let binary_apply a b exp =
    (fold_cps_tag bin_op unary_op nullary_op [@tailcall]) a (fun r_a ->
        (fold_cps_tag bin_op unary_op nullary_op [@tailcall]) b (fun r_b ->
            (bin_op exp) r_a r_b |> cont))
  in
  match x with
  | Mul (_, a, b) -> binary_apply a b x
  | Add (_, a, b) -> binary_apply a b x
  | Sub (_, a, b) -> binary_apply a b x
  | Div (_, a, b) -> binary_apply a b x
  | Sin (_, a) -> unary_apply a x
  | Cos (_, a) -> unary_apply a x
  | Ln (_, a) -> unary_apply a x
  | E (_, a) -> unary_apply a x
  | Sqrt (_, a) -> unary_apply a x
  | Zero _ -> x |> nullary_op |> cont
  | One _ -> x |> nullary_op |> cont
  | Const _ -> x |> nullary_op |> cont
  | Var _ -> x |> nullary_op |> cont

let eval_tag_bianry (exp : 'a expr) left right =
  let left_tag = get_tag left in
  let right_tag = get_tag right in
  match exp with
  | Add _ -> add_tag (left_tag +. right_tag) left right
  | Sub _ -> sub_tag (left_tag -. right_tag) left right
  | Div _ -> div_tag (left_tag /. right_tag) left right
  | Mul _ -> mul_tag (left_tag *. right_tag) left right
  | _ -> failwith binary_warning

let eval_tag_unary (exp : 'a expr) value =
  let tag = get_tag value in
  match exp with
  | Sin _ -> sin_tag (Float.sin tag) value
  | Cos _ -> cos_tag (Float.cos tag) value
  | Ln _ -> ln_tag (Float.log tag) value
  | E _ -> e_tag (Float.exp tag) value
  | Sqrt _ -> sqrt_tag (Float.sqrt tag) value
  | _ -> failwith unary_warning

let eval_tag_nullary env (exp : 'a expr) =
  match exp with
  | Zero -> zero_tag 0.0
  | One -> one_tag 1.0
  | Const a -> const_tag a a
  | Var id -> var_tag (lookup id env) id
  | _ -> failwith nullary_warning

let eval_tag env x =
  fold_cps eval_tag_bianry eval_tag_unary (eval_tag_nullary env) x Base.Fn.id

(* fold over the tree that returns a function *)

(* back propagation is the top down traversal *)
(* top level traversal can be expressed as a function *)
(* i represents the current label *)

(** 
  v = l + r => df/dl = df/dv * dv/dl = df/dv
  v = l - r => df/dr = df/dv * dv/dr = - df/dv
  v = l * r => df/dl = df/dv * r
  v = l/r => df/dl = df/dv * 1/r | df/dr = -1 * df/dv * l * (1 /. r^2)
*)
let backprop_binary v fdl fdr v_derv =
  match v with
  | Add _ -> add_tag v_derv (fdl v_derv) (fdr v_derv)
  | Sub _ -> add_tag v_derv (fdl v_derv) (fdr (Float.neg v_derv))
  | Mul (_, l, r) ->
      mul_tag v_derv (fdl (v_derv *. get_tag r)) (fdr (v_derv *. get_tag l))
  | Div (_, l, r) ->
      let l_v = get_tag l in
      let r_v = get_tag r in
      let dvdl = 1.0 /. r_v in
      let dvdr = Float.neg (l_v /. (r_v *. r_v)) in
      div_tag v_derv (fdl (v_derv *. dvdl)) (fdr (v_derv *. dvdr))
  | _ -> failwith binary_warning

(**
  v = sin x => df/dx = df/dv * dv/dx = cos x * df/dv
  v - cos x => df/dx = df/dv * dv/dx = -sin x * df/dv
  v = ln x => df/dx = df/dv * dv/dx = 1/x * df/dv
  v = e ^ x => df/dx = df/dv * dv/dx = e ^ x * df/dv
  v = sqrt (x) => df/dx = df/dv * dv/dx = df/dv * -0.5 / v
*)
let backprop_unary v fd v_derv =
  match v with
  | Sin (_, x) -> sin_tag v_derv (fd (Float.cos (get_tag x) *. v_derv))
  | Cos (_, x) ->
      cos_tag v_derv (fd (Float.neg (Float.sin (get_tag x) *. v_derv)))
  | Ln (_, x) -> ln_tag v_derv (fd (v_derv /. get_tag x))
  | E (t, _) -> e_tag v_derv (fd (v_derv *. t))
  | Sqrt (t, _) -> sqrt_tag v_derv (fd (v_derv *. 0.5 /. t))
  | _ -> failwith unary_warning

(**
 df/dx = df/dv * dv/dx 
*)
let backprop_nullary v v_derv =
  match v with
  | Zero _ -> zero_tag 0.0
  | One _ -> one_tag 0.0
  | Const (_, v) -> const_tag 0.0 v
  | Var (_, x) -> var_tag v_derv x
  | _ -> failwith nullary_warning

let backprop x =
  (fold_cps_tag backprop_binary backprop_unary backprop_nullary x Base.Fn.id)
    1.0

let collect_result x =
  let collect_result_nullary x =
    match x with Var (tag, id) -> empty |> update id tag | _ -> empty
  in
  let collect_result_unary _ x = x in
  let collect_result_binary _ l r =
    IntMap.union (fun _ a b -> Some (a +. b)) l r
  in
  fold_cps_tag collect_result_binary collect_result_unary collect_result_nullary
    x Base.Fn.id

let backward_all_diff env formula =
  eval_tag env formula |> backprop |> collect_result

let backward_diff env id x =
  x |> backward_all_diff env |> IntMap.find_opt id |> Option.value ~default:0.0

let%test_unit "y = x_0 + (x_0 * x_1), x_0 = 2, x_1 = 3" =
  let formula = add (var 0) (mul (var 0) (var 1)) in
  let env = empty |> update 0 2.0 |> update 1 3.0 in
  [%test_eq: Base.float] (forward_diff env 0 formula) 4.0

let fuzzy_comp a b =
  [%test_pred: Base.float]
    ~message:(Printf.sprintf "actual:%.6f expect:%.6f" a b)
    (Base.Fn.flip ( < ) 0.00001)
    (abs_float (a -. b))

let test_formula () =
  let x_0 = var 0 in
  let x_1 = var 1 in
  let x_01 = mul x_0 x_1 in
  let sin_x_0 = sin x_0 in
  let add_x01_sin = add x_01 sin_x_0 in
  let e = e add_x01_sin in
  let add_1_e = add one e in
  div one add_1_e

let test_formula_result evals formula =
  let comp a b =
    [%test_eq: Base.float] a b;
    a
  in
  let values = List.map (fun f -> f formula) evals in
  let _ = List.fold_right comp (List.tl values) (List.hd values) in
  ()

let%test_unit "test evaluators gives back the same result" =
  let formula = test_formula () in
  let env = empty |> update 0 1.0 |> update 1 1.0 in
  test_formula_result
    [ Base.Fn.compose get_tag (eval_tag env); eval env ]
    formula

let test_can_derv formula =
  let _ = diff 0 formula in
  [%test_eq: Base.float] 0.0 0.0

let%test_unit "const" = [%test_eq: Base.float] (eval empty (Const 1.0)) 1.0
let%test "const" = eval empty (Const 1.0) = 1.0

let test_simple diff_evals formula a derv_a b derv_b =
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

let diff_evaluators = [ symbolic_diff; forward_diff; backward_diff ]

let%test_unit "y=x_0" =
  let formula = var 0 in
  test_simple diff_evaluators formula 3.0 1.0 1.0 0.0

let%test_unit "y=sqrt(x_0)" =
  let formula = sqrt (var 0) in
  test_simple diff_evaluators formula 4.0 0.25 1.0 0.0

let%test_unit "y = x_0 + x_1" =
  let formula = add (var 0) (var 1) in
  test_simple diff_evaluators formula 3.0 1.0 1.0 1.0

let%test_unit "y = x_0 * x_1" =
  let formula = mul (var 0) (var 1) in
  test_simple diff_evaluators formula 3.0 4.0 4.0 3.0

let%test_unit "y = 5 * (x_0 * x_1) + x_0" =
  let formula = add (mul (const 5.0) (mul (var 0) (var 1))) (var 0) in
  test_simple diff_evaluators formula 3.0 21.0 4.0 15.0

let%test_unit "complex formula" =
  test_simple diff_evaluators (test_formula ()) 1.0 (-0.181974) 1.0 (-0.118142)

let%test_unit "backward all" =
  let env = empty |> update 0 1.0 |> update 1 1.0 in
  let all_result = backward_all_diff env (test_formula ()) in
  fuzzy_comp (IntMap.find 0 all_result) (-0.181974);
  fuzzy_comp (IntMap.find 1 all_result) (-0.118142)

let rec fact n cont =
  if n = 0 then cont 1
  else (fact [@tailcall]) (n - 1) (fun fact_n_1 -> fact_n_1 * n |> cont)

let%test "fact cps style" = fact 4 Base.Fn.id = 24
