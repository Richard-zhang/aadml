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
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Add :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Sub :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Div :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Sin : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Cos : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Ln : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | E : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Sqrt : 'tag * ('tag, float) tag_expr -> ('tag, float) tag_expr
  | Var : 'tag * int -> ('tag, float) tag_expr
  | Max :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
  | Min :
      'tag * ('tag, float) tag_expr * ('tag, float) tag_expr
      -> ('tag, float) tag_expr
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
  | Cond :
      'tag * ('tag, bool) tag_expr * ('tag, 'a) tag_expr * ('tag, 'a) tag_expr
      -> ('tag, 'a) tag_expr

type ('tag, 'a) nullary = { nop : 'elt. ('tag, 'elt) tag_expr -> 'a }
type ('tag, 'a) unary = { uop : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a }
type ('tag, 'a) binary = { bop : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a -> 'a }

type ('tag, 'a) ternary = {
  top : 'elt. ('tag, 'elt) tag_expr -> 'a -> 'a -> 'a -> 'a;
}

let dummy_ternary () =
  let top _ = failwith "TODO" in
  { top }

type 'a expr = (unit, 'a) tag_expr
type _ ty = TyFloat : float ty | TyBool : bool ty

let rec tyExpr : type a. (_, a) tag_expr -> a ty = function
  | Const _ -> TyFloat
  | Var _ -> TyFloat
  | Equal _ -> TyBool
  | Not _ -> TyBool
  | And _ -> TyBool
  | Or _ -> TyBool
  | Less _ -> TyBool
  | Mul (_, _, b) -> tyExpr b
  | Add (_, _, b) -> tyExpr b
  | Sub (_, _, b) -> tyExpr b
  | Div (_, _, b) -> tyExpr b
  | Max (_, _, b) -> tyExpr b
  | Min (_, _, b) -> tyExpr b
  | Sin (_, a) -> tyExpr a
  | Cos (_, a) -> tyExpr a
  | Ln (_, a) -> tyExpr a
  | E (_, a) -> tyExpr a
  | Sqrt (_, a) -> tyExpr a
  | Cond (_, _, _, b) -> tyExpr b

type 'tag any = Any : ('tag, 'a) tag_expr -> 'tag any
type ('tag, 'a) cont = { run : 'elt. ('tag, 'elt) tag_expr -> 'a }

let spread (Any a) op = op.run a

let cast : type a b. ('tag, a) tag_expr -> b ty -> ('tag, b) tag_expr option =
 fun expr witness ->
  let single_typ = tyExpr expr in
  match (single_typ, witness) with
  | TyFloat, TyFloat -> Some expr
  | TyBool, TyBool -> Some expr
  | _, _ -> None

let magic_cast : type a b. ('tag, a) tag_expr -> b ty -> ('tag, b) tag_expr =
 fun expr witness ->
  match cast expr witness with Some a -> a | None -> failwith "casting fail"

let cast_to_float x = magic_cast x TyFloat
let cast_to_bool x = magic_cast x TyBool

let rec fold_cps :
    type a.
    (_, _) ternary ->
    (_, _) binary ->
    (_, _) unary ->
    (_, _) nullary ->
    (_, a) tag_expr ->
    (_ -> _) ->
    _ =
 fun ternary_op bin_op unary_op nullary_op x cont ->
  let nullary_apply exp = nullary_op.nop exp |> cont in
  let unary_apply a exp =
    fold_cps ternary_op bin_op unary_op nullary_op a (fun r ->
        (unary_op.uop exp) r |> cont)
  in
  let binary_apply a b exp =
    fold_cps ternary_op bin_op unary_op nullary_op a (fun r_a ->
        fold_cps ternary_op bin_op unary_op nullary_op b (fun r_b ->
            (bin_op.bop exp) r_a r_b |> cont))
  in
  let ternary_apply a b c exp =
    fold_cps ternary_op bin_op unary_op nullary_op a (fun r_a ->
        fold_cps ternary_op bin_op unary_op nullary_op b (fun r_b ->
            fold_cps ternary_op bin_op unary_op nullary_op c (fun r_c ->
                (ternary_op.top exp) r_a r_b r_c |> cont)))
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
  | Const _ -> nullary_apply x
  | Var _ -> nullary_apply x
  | Not (_, a) -> unary_apply a x
  | And (_, a, b) -> binary_apply a b x
  | Or (_, a, b) -> binary_apply a b x
  | Equal (_, a, b) -> binary_apply a b x
  | Less (_, a, b) -> binary_apply a b x
  | Cond (_, a, b, c) -> ternary_apply a b c x

let add a b = Add ((), a, b)
let mul a b = Mul ((), a, b)
let sub a b = Sub ((), a, b)
let div a b = Div ((), a, b)
let cos a = Cos ((), a)
let sin a = Sin ((), a)
let e a = E ((), a)
let ln a = Ln ((), a)
let sqrt a = Sqrt ((), a)
let var id = Var ((), id)
let const a = Const ((), a)
let zero = const 0.0
let one = const 1.0
let neg a = sub zero a

let power time n =
  if time == 0 then one else Base.Fn.apply_n_times ~n:(time - 1) (mul n) n

let eval_nullary env =
  let nop : type a. (_, a) tag_expr -> _ = function
    | Const (_, a) -> a
    | Var (_, id) -> lookup id env
    | _ -> failwith nullary_warning
  in
  { nop }

let eval_unary =
  let uop : type a. (_, a) tag_expr -> _ -> _ = function
    | Sin _ -> Float.sin
    | Cos _ -> Float.cos
    | Ln _ -> Float.log
    | E _ -> Float.exp
    | Sqrt _ -> Float.sqrt
    | _ -> failwith unary_warning
  in
  { uop }

let eval_binary =
  let bop : type a. (_, a) tag_expr -> _ -> _ -> _ = function
    | Add _ -> ( +. )
    | Sub _ -> ( -. )
    | Div _ -> ( /. )
    | Mul _ -> ( *. )
    | Max _ -> Float.max
    | Min _ -> Float.min
    | _ -> failwith binary_warning
  in
  { bop }

let eval_ternary =
  let top : type a. (_, a) tag_expr -> _ -> _ -> _ -> _ = function
    | Cond _ -> fun a b c -> if a == 1. then b else c
    | _ -> failwith ternary_warning
  in
  { top }

let eval env x =
  fold_cps eval_ternary eval_binary eval_unary (eval_nullary env) x Base.Fn.id

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
  | Const (_, a) -> show a
  | Var (_, id) -> string_of_int id
  | Max _ -> "max"
  | Min _ -> "min"
  | Not _ -> "not"
  | And _ -> "and"
  | Or _ -> "or"
  | Equal _ -> "equal"
  | Less _ -> "less"
  | Cond _ -> "If"

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
  | Var (tag, _) -> tag
  | Max (tag, _, _) -> tag
  | Min (tag, _, _) -> tag
  | Not (tag, _) -> tag
  | And (tag, _, _) -> tag
  | Or (tag, _, _) -> tag
  | Equal (tag, _, _) -> tag
  | Less (tag, _, _) -> tag
  | Cond (tag, _, _, _) -> tag

let add_tag tag a b = Add (tag, a, b)
let mul_tag tag a b = Mul (tag, a, b)
let sub_tag tag a b = Sub (tag, a, b)
let div_tag tag a b = Div (tag, a, b)
let cos_tag tag a = Cos (tag, a)
let sin_tag tag a = Sin (tag, a)
let e_tag tag a = E (tag, a)
let ln_tag tag a = Ln (tag, a)
let sqrt_tag tag v = Sqrt (tag, v)
let var_tag tag id = Var (tag, id)
let const_tag tag v = Const (tag, v)
let zero_tag tag = const_tag tag 0.0
let one_tag tag = const_tag tag 1.0
let not_tag tag v = Not (tag, v)

let add_any_tag tag (Any left) (Any right) =
  Any (add_tag tag (cast_to_float left) (cast_to_float right))

let mul_any_tag tag (Any left) (Any right) =
  Any (mul_tag tag (cast_to_float left) (cast_to_float right))

let sub_any_tag tag (Any left) (Any right) =
  Any (sub_tag tag (cast_to_float left) (cast_to_float right))

let div_any_tag tag (Any left) (Any right) =
  Any (div_tag tag (cast_to_float left) (cast_to_float right))

let cos_any_tag tag (Any v) = Any (cos_tag tag (cast_to_float v))
let sin_any_tag tag (Any v) = Any (sin_tag tag (cast_to_float v))
let ln_any_tag tag (Any v) = Any (ln_tag tag (cast_to_float v))
let e_any_tag tag (Any v) = Any (e_tag tag (cast_to_float v))
let sqrt_any_tag tag (Any v) = Any (sqrt_tag tag (cast_to_float v))
let zero_any_tag tag = Any (zero_tag tag)
let one_any_tag tag = Any (one_tag tag)
let var_any_tag tag id = Any (var_tag tag id)
let const_any_tag tag v = Any (const_tag tag v)
let not_any_tag tag (Any v) = Any (not_tag tag (cast_to_bool v))
