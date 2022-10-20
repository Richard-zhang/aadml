[@@@warning "-32"]
open Aadml


let draw_owl () =
  let formula = Example.test_formula () in
  let graph = Visual.graph formula in
  print_endline graph

let draw_bs () =
  let open Expr in
  let name_env = empty |> update 0 "vol" |> update 1 "S" |> update 2 "K" |> update 3 "T" |> update 4 "r" in
  let formula = Bs.bs ~vol:0 ~stock:1 ~strike:2 ~t:3 ~rate:4 in
  Visual.graph ~name_env formula |> print_endline 

let () = draw_bs ()
