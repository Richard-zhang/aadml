[@@@warning "-32"]

open Aadml

let draw_owl () =
  let formula = Example.test_formula () in
  let graph = Visual.graph formula in
  print_endline graph

let draw_owl_intermediate_fd () =
  let open Expr in
  let env = empty |> update 0 1. |> update 1 1. in
  let formula = Example.test_formula () in
  let intermediate_graph = Debug_diff.debug_forward_diff env 1 formula in
  let graph = Visual.float_float_graph intermediate_graph in
  print_endline graph

let draw_owl_intermediate_feedforward () =
  let open Expr in
  let env = empty |> update 0 1. |> update 1 1. in
  let formula = Example.test_formula () in
  let intermediate_graph = Diff.debug_backward_feedforwrad env formula in
  let graph = Visual.float_graph intermediate_graph in
  print_endline graph

let draw_owl_intermediate_backprop () =
  let open Expr in
  let env = empty |> update 0 1. |> update 1 1. in
  let formula = Example.test_formula () in
  let intermediate_graph = Diff.debug_backward_backprop env formula in
  let graph = Visual.float_graph intermediate_graph in
  print_endline graph

let draw_bs () =
  let open Expr in
  let name_env =
    empty |> update 0 "vol" |> update 1 "spot" |> update 2 "strike"
    |> update 3 "time" |> update 4 "rate"
  in
  let formula = Bs.formula ~vol:0 ~stock:1 ~strike:2 ~t:3 ~rate:4 in
  Visual.graph ~name_env formula |> print_endline

let () = draw_owl ()
