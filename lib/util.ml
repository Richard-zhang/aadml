let fuzzy_compare ?(accuracy = 0.00001) a b =
  [%test_pred: Base.float]
    ~message:(Printf.sprintf "actual:%.6f expect:%.6f" a b)
    (Base.Fn.flip ( < ) accuracy)
    (abs_float (a -. b))

let nullary_warning = "nullary operator only"
let unary_warning = "unary operator only"
let binary_warning = "binary operator only"
let ternary_warning = "ternary operator only"
