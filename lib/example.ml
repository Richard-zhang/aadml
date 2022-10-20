let test_formula () =
  let open Expr in
  let x_0 = var 0 in
  let x_1 = var 1 in
  let x_01 = mul x_0 x_1 in
  let sin_x_0 = sin x_0 in
  let add_x01_sin = add x_01 sin_x_0 in
  let e = e add_x01_sin in
  let add_1_e = add one e in
  div one add_1_e
