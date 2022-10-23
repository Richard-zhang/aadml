open Expr

(* if the else *)

(* TODO add control-flow if then else *)
let pay_off_func strike stock_price =
  let profit = sub stock_price strike in
  if profit > zero then profit else zero

(* Fix: let's hack this side effect *)
(* SOL: let's hack this side effect *)
(* closure form with some internal state associated with this function *)
let coin_flip =
  Random.init 1234;
  let f () = if Random.bool () then one else neg one in
  f

(* discrete_model of black schole *)
let s ~time_step ~rate ~vol s_t =
  let risk_free_return = mul rate (mul s_t time_step) in
  let price_disturbance =
    let sqrt_t = sqrt time_step in
    mul vol (mul s_t (mul (coin_flip ()) sqrt_t))
  in
  add s_t (add risk_free_return price_disturbance)

let generate_path ~num_step ~time_step ~rate ~vol ~init =
  let f = s ~time_step ~rate ~vol in
  Base.Fn.apply_n_times ~n:num_step f init

let mc_bs ~num_path ~num_step ~vol ~stock ~strike ~t ~rate =
  let time_step = div t (const (float_of_int num_step)) in
  let gen _ =
    generate_path ~num_step ~time_step ~rate ~vol ~init:stock
    |> pay_off_func strike
  in
  let cashflows = List.init num_path gen in
  let aggregate_cashflows = List.fold_left add zero cashflows in
  let num_cashflows = const (float_of_int num_path) in
  let exp_value = div aggregate_cashflows num_cashflows in
  let df = add one (mul rate time_step) in
  let comp_df = power num_step df in
  div exp_value comp_df
