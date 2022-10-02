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

let rec eval_cps x env f =
  let unary_apply op a = (eval_cps [@tailcall]) a env (Base.Fn.compose f op) in
  let binary_apply op a b =
    (eval_cps [@tailcall]) a env (fun r_a ->
        (eval_cps [@eval_cps]) b env (fun r_b -> op r_a r_b |> f))
  in
  match x with
  | Mul (a, b) -> binary_apply ( *. ) a b
  | Add (a, b) -> binary_apply ( +. ) a b
  | Sub (a, b) -> binary_apply ( -. ) a b
  | Div (a, b) -> binary_apply ( /. ) a b
  | Sin a -> unary_apply sin a
  | Cos a -> unary_apply cos a
  | Ln a -> unary_apply log a
  | E a -> unary_apply exp a
  | Zero -> f 0.0
  | One -> f 1.0
  | Const a -> f a
  | Var int -> f (lookup int env)

let eval x env = eval_cps x env Base.Fn.id
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

let rec diff = function
  | Var x -> fun id -> if id = x then one else zero
  | Zero -> fun _ -> zero
  | One -> fun _ -> zero
  | Const _ -> fun _ -> zero
  | Mul (a, b) -> fun id -> add (mul (diff a id) b) (mul (diff b id) a)
  | Add (a, b) -> fun id -> add (diff a id) (diff b id)
  | Sub (a, b) -> fun id -> sub (diff a id) (diff b id)
  | Div (a, b) ->
      fun id ->
        let u'v = mul (diff a id) b in
        let uv' = mul a (diff b id) in
        div (sub u'v uv') (mul b b)
  | Sin a -> fun id -> mul (cos a) (diff a id)
  | Cos a -> fun id -> sub zero (mul (sin a) (diff a id))
  | E a -> fun id -> mul (e a) (diff a id)
  | Ln a -> fun id -> mul (div one a) (diff a id)

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

let%test_unit "smoke test" =
  let formula = var 0 in
  test_can_derv formula

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
  let complex_formula =
    let x_0 = var 0 in
    let x_1 = var 1 in
    let x_01 = mul x_0 x_1 in
    let sin_x_0 = sin x_0 in
    let add_x01_sin = add x_01 sin_x_0 in
    let e = e add_x01_sin in
    let add_1_e = add one e in
    div one add_1_e
  in
  test_simple complex_formula 1.0 (-0.181974) 1.0 (-0.118142)
