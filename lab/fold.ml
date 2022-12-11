let hello = "hello, world"

let rec right_fold f init = function
  | [] -> init
  | x :: xs -> f x (right_fold f init xs)

let rec left_fold f init = function
  | x :: xs -> left_fold f (f init x) xs
  | [] -> init

(* fold over function *)
  (* 6 2 1 => fun x3 -> f x3 6 |> (fun x2 -> f x2 2 |> (fun x1 -> f x1 1 |> (fun x -> x))) *)
  (* (fun x3 -> f x3 6 |> (fun x2 -> f x2 2 |> (fun x1 -> f x1 1 |> (fun x -> x)))) 1 *)
  (* f 1 6 |> (fun x2 -> f x2 2 |> (fun x1 -> f x1 1 |> (fun x -> x))) *)
  (* (fun x2 -> f x2 2 |> (fun x1 -> f x1 1 |> (fun x -> x))) -5 *)
  (* f -5 2 |> (fun x1 -> f x1 1 |> (fun x -> x)) *)
  (* (fun x1 -> f x1 1 |> (fun x -> x)) -7 *)
  (* f -7 1 |> (fun x -> x) *)
  (* (fun x -> x) -8 *)
  (* -8 *)

(*  a -> (b -> c) *)
let left_fold' f init xs = right_fold (fun elem acc -> (fun x -> f x elem |> acc )) (fun x -> x) xs init

(* think about base case *)

(* replace :: with f operand with fold 6 :: 2 :: 1 :: [] => (f 6 (f 2 (f 1 init))) *)
let%test_unit "right_fold" =
  [%test_eq: Base.int] (right_fold ( - ) 1 [ 6; 2; 1 ]) 4

(* *)
let%test_unit "left_fold'" =
  [%test_eq: Base.int] (left_fold' ( - ) 1 [ 6; 2; 1 ]) (-8)
