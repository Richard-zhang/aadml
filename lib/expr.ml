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
  | Sqrt : 'a expr -> 'a expr
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
  | Sqrt a -> unary_apply a x
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
let sqrt a = Sqrt a
let zero = Zero
let one = One
let var id = Var id
let const a = Const a
let nullary_warning = "nullary operator only"
let unary_warning = "unary operator only"
let binary_warning = "binary operator only"

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

let eval env x = fold_cps eval_binary eval_unary (eval_nullary env) x Base.Fn.id
