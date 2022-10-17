let fuzzy_compare ?(accuracy=0.00001) a b =
  [%test_pred: Base.float]
    ~message:(Printf.sprintf "actual:%.6f expect:%.6f" a b)
    (Base.Fn.flip ( < ) accuracy)
    (abs_float (a -. b))
