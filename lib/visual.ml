open Expr
open Util

type node = Op of int * string | Input of int

let id_of_node = function Op (id, _) -> id | Input id -> id

(* show type class in OCaml *)
let get_node (type a) (show : a -> string) (exp : a expr) label =
  match exp with
  | Var (_, id) -> Input (-1 * id)
  | _ -> Op (label, string_of_op ~show exp)

let float_show = Printf.sprintf "%0.2f"

type label_rs = int -> int * node any

let label_ternary : (unit, label_rs) ternary = failwith "TODO"

(* pre order traversal using label *)
let label_binary =
  let bop : type a. (_, a) tag_expr -> label_rs -> label_rs -> label_rs =
   fun exp fl fr label ->
    let left_label, left = fl (label + 1) in
    let right_label, right = fr (left_label + 1) in
    let result =
      match exp with
      | Add _ -> add_any_tag (get_node float_show exp label) left right
      | Sub _ -> sub_any_tag (get_node float_show exp label) left right
      | Div _ -> div_any_tag (get_node float_show exp label) left right
      | Mul _ -> mul_any_tag (get_node float_show exp label) left right
      | _ -> failwith binary_warning
    in
    (right_label, result)
  in
  { bop }

let label_unary =
  let uop : type a. (_, a) tag_expr -> label_rs -> label_rs =
   fun exp f label ->
    let final_label, value = f (label + 1) in
    let result =
      match exp with
      | Sin _ -> sin_any_tag (get_node float_show exp label) value
      | Cos _ -> cos_any_tag (get_node float_show exp label) value
      | Ln _ -> ln_any_tag (get_node float_show exp label) value
      | E _ -> e_any_tag (get_node float_show exp label) value
      | Sqrt _ -> sqrt_any_tag (get_node float_show exp label) value
      | _ -> failwith unary_warning
    in
    (final_label, result)
  in
  { uop }

let label_nullary =
  let nop : type a. (_, a) tag_expr -> label_rs =
   fun exp label ->
    match exp with
    | Zero _ -> (label, zero_any_tag (get_node float_show exp label))
    | One _ -> (label, one_any_tag (get_node float_show exp label))
    | Const (_, a) -> (label, const_any_tag (get_node float_show exp label) a)
    | Var (_, id) -> (label, var_any_tag (get_node float_show exp label) id)
    | _ -> failwith nullary_warning
  in
  { nop }

let label x =
  fold_cps label_ternary label_binary label_unary label_nullary x (fun f ->
      f 1 |> snd)

type stmt = Edge of int * int | Node of int * string

let string_of_stmt = function
  | Edge (s, e) -> Printf.sprintf "%d -> %d" s e
  | Node (node, label) -> Printf.sprintf "%d[label=\"%s\"];" node label

let stmt_of_node ?(name_env = empty) = function
  | Input id -> (
      match IntMap.find_opt (-id) name_env with
      | Some name -> Node (id, name)
      | None -> Node (id, Printf.sprintf "x%d" (-1 * id)))
  | Op (id, label) -> Node (id, label)

let dot_ternary : (node, stmt list) ternary = failwith "TODO"

let dot_binary =
  let bop : type a. (node, a) tag_expr -> stmt list -> stmt list -> stmt list =
   fun exp a b ->
    let n = get_tag exp in
    let node_id = id_of_node n in
    let gen_stmts left right =
      let l_node = get_tag left |> id_of_node in
      let r_node = get_tag right |> id_of_node in
      [ Edge (l_node, node_id); Edge (r_node, node_id); stmt_of_node n ]
    in
    match exp with
    | Add (_, left, right) -> gen_stmts left right @ a @ b
    | Sub (_, left, right) -> gen_stmts left right @ a @ b
    | Div (_, left, right) -> gen_stmts left right @ a @ b
    | Mul (_, left, right) -> gen_stmts left right @ a @ b
    | _ -> failwith binary_warning
  in
  { bop }

let dot_unary =
  let uop : type a. (node, a) tag_expr -> stmt list -> stmt list =
   fun exp a ->
    let n = get_tag exp in
    let node_id = id_of_node n in
    let gen_stmts sub_exp =
      let sub_node = get_tag sub_exp |> id_of_node in
      [ Edge (sub_node, node_id); stmt_of_node n ]
    in
    match exp with
    | Sin (_, sub_exp) -> gen_stmts sub_exp @ a
    | Cos (_, sub_exp) -> gen_stmts sub_exp @ a
    | Ln (_, sub_exp) -> gen_stmts sub_exp @ a
    | E (_, sub_exp) -> gen_stmts sub_exp @ a
    | Sqrt (_, sub_exp) -> gen_stmts sub_exp @ a
    | _ -> failwith unary_warning
  in
  { uop }

let dot_nullary (name_env : string env) =
  let nop : type a. (node, a) tag_expr -> stmt list =
   fun exp ->
    match exp with
    | Zero tag -> [ stmt_of_node tag ]
    | One tag -> [ stmt_of_node tag ]
    | Const (tag, _) -> [ stmt_of_node tag ]
    | Var (tag, _) -> [ stmt_of_node ~name_env tag ]
    | _ -> failwith nullary_warning
  in
  { nop }

let stmts_of_label_expr ?(name_env = empty) exp =
  fold_cps dot_ternary dot_binary dot_unary (dot_nullary name_env) exp
    Base.Fn.id

let graph_of_stmts stmts =
  let stmts = List.map string_of_stmt stmts in
  let graph = Base.String.concat ~sep:"\n" stmts in
  Printf.sprintf "digraph G {\n%s\n}\n" graph

let graph ?name_env exp =
  let labelled_exp = label exp in
  spread labelled_exp
    {
      run =
        (fun tag_expr ->
          tag_expr |> stmts_of_label_expr ?name_env |> graph_of_stmts);
    }
