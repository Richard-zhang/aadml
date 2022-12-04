[@@@warning "-32-34-37"]

open Expr

let pay_off_func strike stock_price =
  let profit = sub stock_price strike in
  max profit zero

(* Fix: let's hack this side effect *)
(* SOL: let's hack this side effect *)
(* closure form with some internal state associated with this function *)
let gen_coin_flip seed =
  Random.init seed; let f () = if Random.bool () then one else neg one in
  f

let gen_normal_rv _ =
  let t = Pareto.Distributions.Normal.standard in
  let f () = Pareto.Distributions.Normal.random t |> const in
  f

(* discrete_model of black schole *)
type rand_gen_t = unit -> float expr
type model_dynamic_t = rand_gen_t -> float expr -> float expr -> float expr

let bs_model_dynamic_2 ~rate ~vol : model_dynamic_t =
 fun rand_gen delta_t value_at_t ->
  let first = mul (sub rate (mul (const 0.5) (mul vol vol))) delta_t in
  let second = mul vol (mul (sqrt delta_t) (rand_gen ())) in
  mul value_at_t (e (add first second))

let bs_model_dynamic ~rate ~vol : model_dynamic_t =
 fun rand_gen delta_t value_at_t ->
  let risk_free_return = mul rate (mul value_at_t delta_t) in
  let price_disturbance =
    let sqrt_t = sqrt delta_t in
    mul vol (mul value_at_t (mul (rand_gen ()) sqrt_t))
  in
  add value_at_t (add risk_free_return price_disturbance)

let gen_path ~num_step ~init ~time_step
    ~(model_dynamic : float expr -> float expr -> float expr) =
  Base.Fn.apply_n_times ~n:num_step (model_dynamic time_step) init

let get_avg (values : float expr list) =
  let aggregated_values = List.fold_left add zero values in
  let num_cashflows = const (float_of_int (List.length values)) in
  div aggregated_values num_cashflows
(*
let get_discount_factor num_step rate time_step =
  let df = add one (mul rate time_step) in
  let comp_df = power num_step df in
  comp_df
*)

let get_continuous_discount_factor rate t = e (neg (mul rate t))

(* discount factor = (1 + r/m) ^ mt *)
(* possible error here: take discount factor in a continuous way *)
let single_underlying_mc ~(num_path : int) ~(num_step : int) ~(num_seed : int)
    ~(gen_model_dynamic : model_dynamic_t)
    ~(pay_off_func : float expr -> float expr) ~(init : float expr)
    ~(r : float expr) ~(time_in_year : float expr) =
  let coin_flip = gen_normal_rv num_seed in
  let discount_factor = get_continuous_discount_factor r time_in_year in
  let time_step = div time_in_year (const (float_of_int num_step)) in
  let model_dynamic = gen_model_dynamic coin_flip in
  let gen _ =
    let price_at_t = gen_path ~num_step ~init ~time_step ~model_dynamic in
    let future_pay_off = pay_off_func price_at_t in
    future_pay_off
  in
  let paths = List.init num_path gen in
  mul (get_avg paths) discount_factor

(* num_path num_step random_seed are hyperparameter to the monte carlo pricing model
   1. once all hyperparameter fixed, the computational graph associated with this AAD is fixed as well.
   2. vol stock strike t rate are input and part of computation graph
   3. another way to understand hyperparameter is that we're not interested in the derivative of present value with respect to hyperparameters
*)
let mc_bs_impl num_seed num_path num_step ~vol ~stock ~strike ~t ~rate =
  let gen_model_dynamic = bs_model_dynamic_2 ~rate ~vol in
  let pay_off_func = pay_off_func strike in
  single_underlying_mc ~num_path ~num_step ~num_seed ~gen_model_dynamic
    ~pay_off_func ~init:stock ~r:rate ~time_in_year:t

let mc_bs = mc_bs_impl 4321 30000 52

let formula ~vol ~stock ~strike ~t ~rate =
  let vol = var vol in
  let stock = var stock in
  let strike = var strike in
  let t = var t in
  let rate = var rate in
  mc_bs ~vol ~stock ~strike ~t ~rate
