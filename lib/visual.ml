open Expr
open Util

type 'tag node = Op of int * string * 'tag | Input of int * 'tag

let id_of_node = function Op (id, _, _) -> id | Input (id, _) -> id

(* show type class in OCaml *)
let get_node (type tag a) (show : a -> string) (exp : (tag, a) tag_expr) label =
  match exp with
  | Var (_, id) -> Input (-1 * id, get_tag exp)
  | _ -> Op (label, string_of_op ~show exp, get_tag exp)

let float_show = Printf.sprintf "%0.2f"

type 'tag label_rs = int -> int * 'tag node any

(* pre order traversal using label *)
let label_binary =
  let bop : type a. (_, a) tag_expr -> _ label_rs -> _ label_rs -> _ label_rs =
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
  let uop : type a. (_, a) tag_expr -> _ label_rs -> _ label_rs =
   fun exp f label ->
    let final_label, value = f (label + 1) in
    let result =
      match exp with
      | Sin _ -> sin_any_tag (get_node float_show exp label) value
      | Cos _ -> cos_any_tag (get_node float_show exp label) value
      | Ln _ -> ln_any_tag (get_node float_show exp label) value
      | E _ -> e_any_tag (get_node float_show exp label) value
      | Sqrt _ -> sqrt_any_tag (get_node float_show exp label) value
      | Erf _ -> erf_any_tag (get_node float_show exp label) value
      | _ -> failwith unary_warning
    in
    (final_label, result)
  in
  { uop }

let label_nullary =
  let nop : type a. (_, a) tag_expr -> _ label_rs =
   fun exp label ->
    match exp with
    | Const (_, a) -> (label, const_any_tag (get_node float_show exp label) a)
    | Var (_, id) -> (label, var_any_tag (get_node float_show exp label) id)
    | _ -> failwith nullary_warning
  in
  { nop }

let label : type tag. (tag, _) tag_expr -> tag node any =
 fun x ->
  let assign : type a. a label_rs -> a node any = fun f -> f 1 |> snd in
  fold_cps (dummy_ternary ()) label_binary label_unary label_nullary x assign

type 'tag stmt = Edge of int * int | Node of int * string * 'tag

let stmt_of_node ?(name_env = empty) = function
  | Input (id, tag) -> (
      match IntMap.find_opt (-id) name_env with
      | Some name -> Node (id, name, tag)
      | None -> Node (id, Printf.sprintf "x%d" (-1 * id), tag))
  | Op (id, label, tag) -> Node (id, label, tag)

let dot_binary =
  let bop :
      type a.
      ('tag node, a) tag_expr ->
      'tag stmt list ->
      'tag stmt list ->
      'tag stmt list =
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
  let uop : type a. ('tag node, a) tag_expr -> 'tag stmt list -> 'tag stmt list
      =
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
    | Erf (_, sub_exp) -> gen_stmts sub_exp @ a
    | _ -> failwith unary_warning
  in
  { uop }

let dot_nullary (name_env : string env) =
  let nop : type a. ('tag node, a) tag_expr -> 'tag stmt list =
   fun exp ->
    match exp with
    | Const (tag, _) -> [ stmt_of_node tag ]
    | Var (tag, _) -> [ stmt_of_node ~name_env tag ]
    | _ -> failwith nullary_warning
  in
  { nop }

let stmts_of_label_expr ?(name_env = empty) exp =
  fold_cps (dummy_ternary ()) dot_binary dot_unary (dot_nullary name_env) exp
    Base.Fn.id

let string_of_stmt show = function
  | Edge (s, e) -> Printf.sprintf "%d -> %d" s e
  | Node (node, label, tag) ->
      Printf.sprintf "%d[label=<%s<BR/>%s>];" node label (show tag)

let graph_of_stmts show stmts =
  let stmts = List.map (string_of_stmt show) stmts in
  let graph = Base.String.concat ~sep:"\n" stmts in
  Printf.sprintf "digraph G {\n%s\n}\n" graph

let tag_graph (type a) ~(show : a -> string) ?name_env
    (exp : (a, float) tag_expr) =
  let labelled_exp = label exp in
  spread labelled_exp
    {
      run =
        (fun tag_expr ->
          tag_expr |> stmts_of_label_expr ?name_env |> graph_of_stmts show);
    }

let graph ?name_env exp = tag_graph ~show:(fun _ -> "") ?name_env exp

let float_tag_show =
  Printf.sprintf {|<FONT POINT-SIZE="6" FACE="Fira Code">%.3f</FONT>|}

let float_graph ?name_env exp = tag_graph ~show:float_tag_show ?name_env exp

let float_float_tag_show (a, b) =
  Printf.sprintf {|<FONT POINT-SIZE="6" FACE="Fira Code">%.3f, %.3f</FONT>|} a b

let float_float_graph ?name_env exp =
  tag_graph ~show:float_float_tag_show ?name_env exp
