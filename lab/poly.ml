type 'a nested = List of 'a list | Nested of 'a list nested

let rec depth : type a. a nested -> _ = function
  | List _ -> 1
  | Nested n -> 1 + depth n

let len nested =
  let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0 in
  let rec len : type a. (a list -> int) -> a nested -> int =
   fun nested_len n ->
    match n with
    | List l -> nested_len l
    | Nested n -> len (map_and_sum nested_len) n
  in
  len List.length nested

let shape n =
  let rec shape :
      type a b.
      (a nested -> int nested) ->
      (b list list -> a list) ->
      b nested ->
      int nested =
   fun nest nested_shape -> function
    | List _ -> raise (Invalid_argument "gg")
    | Nested (List l) -> nest @@ List (nested_shape l)
    | Nested n ->
        let nested_shape = List.map nested_shape in
        let nest x = nest (Nested x) in
        shape nest nested_shape n
  in
  shape (fun n -> n) (fun l -> List.map List.length l) n

let average_depth x y = (depth x + depth y) / 2

type 'a reduction = { f : 'elt. 'elt nested -> 'a }

let average (reduc : int reduction) x y = (reduc.f x + reduc.f y) / 2

type _ t =
  | Unit : unit t
  | IsUnit : 'a t -> bool t
  | Cast : bool t -> 'a t
  | True : bool t
  | Wrap : 'a t -> 'a t

(* let rec fold : type a z q. (z t -> 'b -> 'b) -> (q t -> 'b) -> a t -> 'b = *)

type 'a unary = { f : 'elt. 'elt t -> 'a -> 'a }
type 'a nullary = { f : 'elt. 'elt t -> 'a }

let rec fold : type a. _ unary -> _ nullary -> a t -> _ =
 fun unary nullary expr ->
  match expr with
  | Unit -> nullary.f expr
  | True -> nullary.f expr
  | Wrap v ->
      let sub = fold unary nullary v in
      unary.f expr sub
  | IsUnit v ->
      let sub = fold unary nullary v in
      unary.f expr sub
  | Cast v ->
      let sub = fold unary nullary v in
      unary.f expr sub

let box_nullary : string nullary =
  let print_nullary (type a) (expr : a t) =
    match expr with Unit -> "unit" | True -> "true" | _ -> "gg"
  in
  { f = print_nullary }

let box_unary : string unary =
  let print_unary (type a) (expr : a t) str =
    match expr with
    | Wrap _ -> "Wrap " ^ str
    | IsUnit _ -> "IsUnit " ^ str
    | Cast _ -> "Cast " ^ str
    | _ -> str
  in
  { f = print_unary }

let expr = IsUnit (Cast (Wrap True))
let () = print_endline @@ fold box_unary box_nullary expr
