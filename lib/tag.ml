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
