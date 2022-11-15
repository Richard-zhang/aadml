[@@@warning "-32-34-37"]

open Expr
open Util

let diff_ternary : (_, 'a) ternary = dummy_ternary ()

let diff_nullary id =
  let nop : type a. (_, a) tag_expr -> unit any = function
    | Var (_, x) -> if id = x then one_any_tag () else zero_any_tag ()
    | Const _ -> zero_any_tag ()
    | _ -> failwith nullary_warning
  in
  { nop }

(* v=f(u(x)) where f is the primitive operation => dv/dx = df/du * du/dx *)
let diff_unary =
  let uop : type a b. (_, a) tag_expr -> unit any -> unit any =
   fun exp dudx ->
    let diff_unary_help = function
      | Sin (_, a) -> cos a
      | Cos (_, a) -> sub zero (sin a)
      | E (_, a) -> e a
      | Ln (_, a) -> div one a
      | Sqrt (_, a) -> div one (mul (add one one) (sqrt a))
      | _ -> failwith unary_warning
    in
    mul_any_tag () (Any (exp |> cast_to_float |> diff_unary_help)) dudx
  in
  { uop }

(* v = l(x) `op` r(x) => dv/dx = ... *)
let diff_binary =
  let bop : type a b. a expr -> unit any -> unit any -> unit any =
   fun x a_dot b_dot ->
    match x with
    | Add _ -> add_any_tag () a_dot b_dot
    | Sub _ -> sub_any_tag () a_dot b_dot
    | Mul (_, a, b) ->
        add_any_tag ()
          (mul_any_tag () a_dot (Any b))
          (mul_any_tag () (Any a) b_dot)
    | Div (_, a, b) ->
        let cast_b = Any b in
        let cast_a = Any a in
        let u'v = mul_any_tag () a_dot cast_b in
        let uv' = mul_any_tag () cast_a b_dot in
        div_any_tag () (sub_any_tag () u'v uv') (mul_any_tag () cast_b cast_b)
    | _ -> failwith binary_warning
  in
  { bop }

let diff id x =
  let any_formula =
    fold_cps diff_ternary diff_binary diff_unary (diff_nullary id) x Base.Fn.id
  in
  spread any_formula { run = cast_to_float }

let symbolic_diff env id formula =
  let diff_formula = diff id formula in
  eval env diff_formula

type forward_t = float * float

let combine_eval_diff_nullary env id =
  let nop : type a. (_, a) tag_expr -> _ =
   fun op ->
    match op with
    | Const (_, a) -> (a, 0.0)
    | Var (_, x) -> (lookup x env, if id = x then 1.0 else 0.0)
    | _ -> failwith nullary_warning
  in
  { nop }

(* merge eval_unary with eval_diff_unary *)
(* d(f(v))/dx = d(f(v))/dv * dv/dx = f'(prev_val) * prev_diff *)
let combine_eval_diff_unary : (_, float * float) unary =
  let uop : type a. (_, a) tag_expr -> _ -> _ =
   fun op ->
    let helper eval diff (prev_val, prev_diff) =
      (eval prev_val, prev_diff *. diff prev_val)
    in
    match op with
    | Sin _ -> helper Float.sin Float.cos
    | Cos _ -> helper Float.cos (Base.Fn.compose Float.neg Float.sin)
    | E _ -> helper Float.exp Float.exp
    | Ln _ -> helper Float.log (( /. ) 1.0)
    | Sqrt _ -> helper Float.sqrt (Base.Fn.compose (( /. ) 0.5) Float.sqrt)
    | _ -> failwith unary_warning
  in
  { uop }

(* v = left `op` right => dv/dx = *)
let combine_eval_diff_binary =
  let bop : type a. (_, a) tag_expr -> _ -> _ -> _ =
   fun op ->
    let helper eval diff (left_val, left_diff) (right_val, right_diff) =
      ( eval left_val right_val,
        diff (left_val, left_diff) (right_val, right_diff) )
    in
    match op with
    | Add _ -> helper ( +. ) (fun (_, x') (_, y') -> x' +. y')
    | Sub _ -> helper ( -. ) (fun (_, x') (_, y') -> x' -. y')
    | Mul _ -> helper ( *. ) (fun (x, x') (y, y') -> (x' *. y) +. (x *. y'))
    | Div _ ->
        helper ( /. ) (fun (x, x') (y, y') ->
            let u'v = x' *. y in
            let uv' = x *. y' in
            (u'v -. uv') /. (y *. y))
    | _ -> failwith binary_warning
  in
  { bop }

let combine_eval_diff_ternary : (_, float * float) ternary = dummy_ternary ()

(*
  forword diff calculate forward and diff at the same time 
*)
let forward_diff env id x =
  fold_cps combine_eval_diff_ternary combine_eval_diff_binary
    combine_eval_diff_unary
    (combine_eval_diff_nullary env id)
    x snd

(*
backward diff is two pass
1. forward pass: annotate the value at each operation
2. back propagation: traverse the graph backwards
*)

(* is there a way work around with this problem *)
let eval_tag_bianry =
  let bop : type a. a expr -> float any -> float any -> float any =
   fun exp left right ->
    let left_tag = spread left { run = get_tag } in
    let right_tag = spread right { run = get_tag } in
    match exp with
    | Add _ -> add_any_tag (left_tag +. right_tag) left right
    | Sub _ -> sub_any_tag (left_tag -. right_tag) left right
    | Div _ -> div_any_tag (left_tag /. right_tag) left right
    | Mul _ -> mul_any_tag (left_tag *. right_tag) left right
    | _ -> failwith binary_warning
  in
  { bop }

(* need a type witness on existentially quantified type *)
let eval_tag_unary =
  let uop : type a. a expr -> float any -> float any =
   fun exp (Any value) ->
    let tag = get_tag value in
    match exp with
    | Not _ -> Any (not_tag (Float.neg tag) (cast_to_bool value))
    | Sin _ -> Any (sin_tag (Float.sin tag) (cast_to_float value))
    | Cos _ -> Any (cos_tag (Float.cos tag) (cast_to_float value))
    | Ln _ -> Any (ln_tag (Float.log tag) (cast_to_float value))
    | E _ -> Any (e_tag (Float.exp tag) (cast_to_float value))
    | Sqrt _ -> Any (sqrt_tag (Float.sqrt tag) (cast_to_float value))
    | _ -> failwith unary_warning
  in
  { uop }

let eval_tag_nullary env =
  let nop : type a. a expr -> float any =
   fun exp ->
    match exp with
    | Const (_, a) -> Any (const_tag a a)
    | Var (_, id) -> Any (var_tag (lookup id env) id)
    | _ -> failwith nullary_warning
  in
  { nop }

let eval_tag_ternary : (_, float any) ternary = dummy_ternary ()

let eval_tag env x =
  fold_cps eval_tag_ternary eval_tag_bianry eval_tag_unary
    (eval_tag_nullary env) x Base.Fn.id

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

(**
1. top level traverl is implemented as a fold over function
2. the traversal start with dervivative = 1.0
3. the propogate the change accordingly
*)

type b_rs = float -> float any

let backprop_binary =
  let bop : type a. (_, a) tag_expr -> b_rs -> b_rs -> b_rs =
   fun v fdl fdr v_derv ->
    match v with
    | Add _ -> add_any_tag v_derv (fdl v_derv) (fdr v_derv)
    | Sub _ -> add_any_tag v_derv (fdl v_derv) (fdr (Float.neg v_derv))
    | Mul (_, l, r) ->
        mul_any_tag v_derv
          (fdl (v_derv *. get_tag r))
          (fdr (v_derv *. get_tag l))
    | Div (_, l, r) ->
        let l_v = get_tag l in
        let r_v = get_tag r in
        let dvdl = 1.0 /. r_v in
        let dvdr = Float.neg (l_v /. (r_v *. r_v)) in
        div_any_tag v_derv (fdl (v_derv *. dvdl)) (fdr (v_derv *. dvdr))
    | _ -> failwith binary_warning
  in
  { bop }

(**
  v = sin x => df/dx = df/dv * dv/dx = cos x * df/dv
  v - cos x => df/dx = df/dv * dv/dx = -sin x * df/dv
  v = ln x => df/dx = df/dv * dv/dx = 1/x * df/dv
  v = e ^ x => df/dx = df/dv * dv/dx = e ^ x * df/dv
  v = sqrt (x) => df/dx = df/dv * dv/dx = df/dv * -0.5 / v
*)
let backprop_unary =
  let uop : type a. (_, a) tag_expr -> b_rs -> b_rs =
   fun v fd v_derv ->
    match v with
    | Sin (_, x) -> sin_any_tag v_derv (fd (Float.cos (get_tag x) *. v_derv))
    | Cos (_, x) ->
        cos_any_tag v_derv (fd (Float.neg (Float.sin (get_tag x) *. v_derv)))
    | Ln (_, x) -> ln_any_tag v_derv (fd (v_derv /. get_tag x))
    | E (t, _) -> e_any_tag v_derv (fd (v_derv *. t))
    | Sqrt (t, _) -> sqrt_any_tag v_derv (fd (v_derv *. 0.5 /. t))
    | _ -> failwith unary_warning
  in
  { uop }

(**
 v = x => df/dx = df/dv * dv/dx 
*)
let backprop_nullary =
  let nop : type a. (_, a) tag_expr -> b_rs =
   fun v v_derv ->
    match v with
    | Const (_, v) -> const_any_tag 0.0 v
    | Var (_, x) -> var_any_tag v_derv x
    | _ -> failwith nullary_warning
  in
  { nop }

let backprop_ternary : ('tag, b_rs) ternary = dummy_ternary ()

let backprop x =
  (fold_cps backprop_ternary backprop_binary backprop_unary backprop_nullary x
     Base.Fn.id)
    1.0

let collect_result x =
  let collect_result_ternary : (_, float env) ternary = dummy_ternary () in
  let collect_result_nullary : (_, float env) nullary =
    let nop : type a. ('tag, a) tag_expr -> float env =
     fun x ->
      match x with Var (tag, id) -> empty |> update id tag | _ -> empty
    in
    { nop }
  in
  let collect_result_unary : (_, float env) unary =
    let uop _ env = env in
    { uop }
  in
  let collect_result_binary : (_, float env) binary =
    let bop _ l r = IntMap.union (fun _ a b -> Some (a +. b)) l r in
    { bop }
  in
  fold_cps collect_result_ternary collect_result_binary collect_result_unary
    collect_result_nullary x Base.Fn.id

let backward_all_diff env formula =
  let intermediate_result = eval_tag env formula in
  let backprop_result = spread intermediate_result { run = backprop } in
  let result = spread backprop_result { run = collect_result } in
  result

let backward_diff env id x =
  x |> backward_all_diff env |> IntMap.find_opt id |> Option.value ~default:0.0

let%test_unit "y = x_0 + (x_0 * x_1), x_0 = 2, x_1 = 3" =
  let formula = add (var 0) (mul (var 0) (var 1)) in
  let env = empty |> update 0 2.0 |> update 1 3.0 in
  [%test_eq: Base.float] (forward_diff env 0 formula) 4.0

let test_formula_result evals formula =
  let comp a b =
    [%test_eq: Base.float] a b;
    a
  in
  let values = List.map (fun f -> f formula) evals in
  let _ = List.fold_right comp (List.tl values) (List.hd values) in
  ()

let%test_unit "test evaluators gives back the same result" =
  let formula = Example.test_formula () in
  let env = empty |> update 0 1.0 |> update 1 1.0 in
  test_formula_result
    [
      (fun formula -> spread (eval_tag env formula) { run = get_tag }); eval env;
    ]
    formula

let test_can_derv formula =
  let _ = diff 0 formula in
  [%test_eq: Base.float] 0.0 0.0

let%test_unit "const" = [%test_eq: Base.float] (eval empty (const 1.0)) 1.0
let%test "const" = eval empty (const 1.0) = 1.0

let test_simple diff_evals formula a derv_a b derv_b =
  let env = empty |> update 0 a |> update 1 b in
  let _ =
    List.map
      (fun diff_eval -> Util.fuzzy_compare (diff_eval env 0 formula) derv_a)
      diff_evals
  in
  let _ =
    List.map
      (fun diff_eval -> Util.fuzzy_compare (diff_eval env 1 formula) derv_b)
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
  test_simple diff_evaluators (Example.test_formula ()) 1.0 (-0.181974) 1.0
    (-0.118142)

let%test_unit "backward all" =
  let env = empty |> update 0 1.0 |> update 1 1.0 in
  let all_result = backward_all_diff env (Example.test_formula ()) in
  Util.fuzzy_compare (IntMap.find 0 all_result) (-0.181974);
  Util.fuzzy_compare (IntMap.find 1 all_result) (-0.118142)

let rec fact n cont =
  if n = 0 then cont 1
  else (fact [@tailcall]) (n - 1) (fun fact_n_1 -> fact_n_1 * n |> cont)

let%test "fact cps style" = fact 4 Base.Fn.id = 24
