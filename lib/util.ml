let fuzzy_compare a b =
  [%test_pred: Base.float]
    ~message:(Printf.sprintf "actual:%.6f expect:%.6f" a b)
    (Base.Fn.flip ( < ) 0.00001)
    (abs_float (a -. b))
