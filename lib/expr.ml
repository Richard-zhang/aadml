open Util

[@@@warning "-32-34-37"]

module IntMap = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

type 'a env = 'a IntMap.t

let empty = IntMap.empty
let update = IntMap.add
let lookup key = IntMap.find key

type ('tag, _) tag_expr =
  | Const : 'tag * float -> ('tag, float) tag_expr
  | Mul :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Add :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Sub :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Div :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Sin : 'tag * ('tag, 'a) tag_expr -> ('tag, 'a) tag_expr
  | Cos : 'tag * ('tag, 'a) tag_expr -> ('tag, 'a) tag_expr
  | Ln : 'tag * ('tag, 'a) tag_expr -> ('tag, 'a) tag_expr
  | E : 'tag * ('tag, 'a) tag_expr -> ('tag, 'a) tag_expr
  | Sqrt : 'tag * ('tag, 'a) tag_expr -> ('tag, 'a) tag_expr
  | Zero : 'tag -> ('tag, 'a) tag_expr
  | One : 'tag -> ('tag, 'a) tag_expr
  | Var : 'tag * int -> ('tag, 'a) tag_expr
  | Max :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Min :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr
  | Not : 'tag * ('tag, bool) tag_expr -> ('tag, bool) tag_expr
  | And :
      'tag * ('tag, bool) tag_expr * ('tag, bool) tag_expr
      -> ('tag, bool) tag_expr
  | Or :
      'tag * ('tag, bool) tag_expr * ('tag, bool) tag_expr
      -> ('tag, bool) tag_expr
  | Equal :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, bool) tag_expr
  | Less :
      'tag * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, bool) tag_expr
  | IfThenElse :
      'tag * ('tag, bool) tag_expr * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr

type ('tag, 'a) nullary = { op : 'elt. ('tag, 'elt) tag_expr -> 'a }
type ('tag, 'a) unary = { op : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a }
type ('tag, 'a) binary = { op : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a -> 'a }
type 'a expr = (unit, 'a) tag_expr

let rec fold_cps :
    type a.
    (_, _) binary ->
    (_, _) unary ->
    (_, _) nullary ->
    (_, a) tag_expr ->
    (_ -> _) ->
    _ =
 fun bin_op unary_op nullary_op x cont ->
  let nullary_apply exp = nullary_op.op exp |> cont in
  let unary_apply a exp =
    fold_cps bin_op unary_op nullary_op a (fun r -> (unary_op.op exp) r |> cont)
  in
  let binary_apply a b exp =
    fold_cps bin_op unary_op nullary_op a (fun r_a ->
        fold_cps bin_op unary_op nullary_op b (fun r_b ->
            (bin_op.op exp) r_a r_b |> cont))
  in
  match x with
  | Mul (_, a, b) -> binary_apply a b x
  | Add (_, a, b) -> binary_apply a b x
  | Sub (_, a, b) -> binary_apply a b x
  | Div (_, a, b) -> binary_apply a b x
  | Max (_, a, b) -> binary_apply a b x
  | Min (_, a, b) -> binary_apply a b x
  | Sin (_, a) -> unary_apply a x
  | Cos (_, a) -> unary_apply a x
  | Ln (_, a) -> unary_apply a x
  | E (_, a) -> unary_apply a x
  | Sqrt (_, a) -> unary_apply a x
  | Zero _ -> nullary_apply x
  | One _ -> nullary_apply x
  | Const _ -> nullary_apply x
  | Var _ -> nullary_apply x
  | Not (_, a) -> unary_apply a x
  | And (_, a, b) -> binary_apply a b x
  | Or (_, a, b) -> binary_apply a b x
  (*     | Equal (a, b) -> binary_apply a b x *)
  | _ -> failwith "TODO"

let add a b = Add ((), a, b)
let mul a b = Mul ((), a, b)
let sub a b = Sub ((), a, b)
let div a b = Div ((), a, b)
let cos a = Cos ((), a)
let sin a = Sin ((), a)
let e a = E ((), a)
let ln a = Ln ((), a)
let sqrt a = Sqrt ((), a)
let zero = Zero ()
let one = One ()
let var id = Var ((), id)
let const a = Const ((), a)
let neg a = sub zero a

let power time n =
  if time == 0 then one else Base.Fn.apply_n_times ~n:(time - 1) (mul n) n

module Eq = struct
  type (_, _) t = Refl : ('a, 'a) t
end

let eval_nullary : type b. float env -> (b, float) nullary =
 fun env ->
  let op : type a. (b, a) tag_expr -> float = function
    | Zero _ -> 0.0
    | One _ -> 1.0
    | Const (_, a) -> a
    | Var (_, id) -> lookup id env
    | _ -> failwith nullary_warning
  in
  { op }

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

let eval env x =
  fold_cps { op = eval_binary } { op = eval_unary } (eval_nullary env) x
    Base.Fn.id

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
  | Zero _ -> "0"
  | One _ -> "1"
  | Const (_, a) -> show a
  | Var (_, id) -> string_of_int id
  | Max _ -> "max"
  | Min _ -> "min"
  | Not _ -> "not"
  | And _ -> "and"
  | Or _ -> "or"
  | Equal _ -> "equal"
  | Less _ -> "less"
  | IfThenElse _ -> "If"

let get_tag : type a. (_, a) tag_expr -> _ = function
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
  | Max (tag, _, _) -> tag
  | Min (tag, _, _) -> tag
  | Not (tag, _) -> tag
  | And (tag, _, _) -> tag
  | Or (tag, _, _) -> tag
  | Equal (tag, _, _) -> tag
  | Less (tag, _, _) -> tag
  | IfThenElse (tag, _, _, _) -> tag

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
