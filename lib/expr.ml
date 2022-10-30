open Util

[@@@warning "-32-34-37-69"]

module IntMap = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

type 'a env = 'a IntMap.t

let empty = IntMap.empty
let update = IntMap.add
let lookup key = IntMap.find key

module type Tag = sig
  type t
end

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
  | Max : 'a expr * 'a expr -> 'a expr
  | Min : 'a expr * 'a expr -> 'a expr
  | Not : bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Equal : 'a expr * 'a expr -> bool expr
  | Less : 'a expr * 'a expr -> bool expr
  | IfThenElse : bool expr * 'a expr * 'a expr -> 'a expr

type 'a binary = { op : 'elt. 'elt expr -> 'a -> 'a -> 'a }
type 'a unary = { op : 'elt. 'elt expr -> 'a -> 'a }
type 'a nullary = { op : 'elt. 'elt expr -> 'a }

let rec fold_cps :
    type a.
    (a expr -> _ -> _ -> _) ->
    (a expr -> _ -> _) ->
    (a expr -> _) ->
    a expr ->
    (_ -> _) ->
    _ =
 fun bin_op unary_op nullary_op x cont ->
  let nullary_apply exp = nullary_op exp |> cont in
  let unary_apply a exp =
    fold_cps bin_op unary_op nullary_op a (fun r -> (unary_op exp) r |> cont)
  in
  let binary_apply a b exp =
    fold_cps bin_op unary_op nullary_op a (fun r_a ->
        fold_cps bin_op unary_op nullary_op b (fun r_b ->
            (bin_op exp) r_a r_b |> cont))
  in
  match x with
  | Mul (a, b) -> binary_apply a b x
  | Add (a, b) -> binary_apply a b x
  | Sub (a, b) -> binary_apply a b x
  | Div (a, b) -> binary_apply a b x
  | Max (a, b) -> binary_apply a b x
  | Min (a, b) -> binary_apply a b x
  | Sin a -> unary_apply a x
  | Cos a -> unary_apply a x
  | Ln a -> unary_apply a x
  | E a -> unary_apply a x
  | Sqrt a -> unary_apply a x
  | Zero -> nullary_apply x
  | One -> nullary_apply x
  | Const _ -> nullary_apply x
  | Var _ -> nullary_apply x
  | Not a -> unary_apply a x
  | And (a, b) -> binary_apply a b x
  | Or (a, b) -> binary_apply a b x
  (*     | Equal (a, b) -> binary_apply a b x *)
  | _ -> failwith "g"

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
let neg a = sub zero a

let power time n =
  if time == 0 then one else Base.Fn.apply_n_times ~n:(time - 1) (mul n) n

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
  | Max _ -> Float.max
  | Min _ -> Float.min
  | _ -> failwith binary_warning

let eval env x = fold_cps eval_binary eval_unary (eval_nullary env) x Base.Fn.id

let string_of_op (type a) ~(show : a -> string) (exp : a expr) =
  match exp with
  | Add _ -> "+"
  | Mul _ -> "*"
  | Sub _ -> "-"
  | Div _ -> "/"
  | Sin _ -> "sin"
  | Cos _ -> "cos"
  | Ln _ -> "ln"
  | E _ -> "e"
  | Sqrt _ -> "sqrt"
  | Zero -> "0"
  | One -> "1"
  | Const a -> show a
  | Var id -> string_of_int id
  | _ -> failwith "G"
