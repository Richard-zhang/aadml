open Expr
open Util

type forward_t = (float * float, float) tag_expr

let erf_coef = 2. /. Float.sqrt Float.pi

let combine_eval_diff_nullary env id : (_, forward_t) nullary =
  let nop : type a. (_, a) tag_expr -> forward_t =
   fun op ->
    match op with
    | Const (_, a) -> const_tag (a, 0.0) a
    | Var (_, x) -> var_tag (lookup x env, if id = x then 1.0 else 0.0) x
    | _ -> failwith nullary_warning
  in
  { nop }

(* merge eval_unary with eval_diff_unary *)
(* d(f(v))/dx = d(f(v))/dv * dv/dx = f'(prev_val) * prev_diff *)
let combine_eval_diff_unary : (_, forward_t) unary =
  let uop : type a. (_, a) tag_expr -> forward_t -> forward_t =
   fun op ->
    let helper eval diff mk_node node =
      let prev_val, prev_diff = get_tag node in
      mk_node (eval prev_val, prev_diff *. diff prev_val) node
    in
    match op with
    | Sin _ -> helper Float.sin Float.cos sin_tag
    | Cos _ -> helper Float.cos (Base.Fn.compose Float.neg Float.sin) cos_tag
    | E _ -> helper Float.exp Float.exp e_tag
    | Ln _ -> helper Float.log (( /. ) 1.0) ln_tag
    | Sqrt _ ->
        helper Float.sqrt (Base.Fn.compose (( /. ) 0.5) Float.sqrt) sqrt_tag
    | Erf _ ->
        helper Float.erf
          (fun x ->
            let value = Float.exp (Float.neg (x *. x)) in
            value *. erf_coef)
          erf_tag
    | _ -> failwith unary_warning
  in
  { uop }

(**
  v = left `op` right => dv/dx = 
  max will choose greater value and identity   
  min will choose smaller value and identity
**)
let combine_eval_diff_binary : (_, forward_t) binary =
  let bop : type a. (_, a) tag_expr -> forward_t -> forward_t -> forward_t =
   fun op ->
    let helper eval diff mk_node left_node right_node =
      let left_val, left_diff = get_tag left_node in
      let right_val, right_diff = get_tag right_node in
      let new_tag =
        ( eval left_val right_val,
          diff (left_val, left_diff) (right_val, right_diff) )
      in
      mk_node new_tag left_node right_node
    in
    match op with
    | Add _ -> helper ( +. ) (fun (_, x') (_, y') -> x' +. y') add_tag
    | Sub _ -> helper ( -. ) (fun (_, x') (_, y') -> x' -. y') sub_tag
    | Mul _ ->
        helper ( *. ) (fun (x, x') (y, y') -> (x' *. y) +. (x *. y')) mul_tag
    | Div _ ->
        helper ( /. )
          (fun (x, x') (y, y') ->
            let u'v = x' *. y in
            let uv' = x *. y' in
            (u'v -. uv') /. (y *. y))
          div_tag
    | Max _ ->
        helper Float.max
          (fun (x, x') (y, y') -> if x > y then x' else y')
          max_tag
    | Min _ ->
        helper Float.min
          (fun (x, x') (y, y') -> if x < y then x' else y')
          min_tag
    | _ -> failwith binary_warning
  in
  { bop }

let combine_eval_diff_ternary : (_, forward_t) ternary = dummy_ternary ()

let debug_forward_diff env id x =
  fold_cps combine_eval_diff_ternary combine_eval_diff_binary
    combine_eval_diff_unary
    (combine_eval_diff_nullary env id)
    x Base.Fn.id
