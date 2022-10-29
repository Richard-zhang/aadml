(*  Fixpoints and Recursion https://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec29-fixpoints/lec29.html *)

module type FAlgebra = sig
  type _ t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module Fix (M : FAlgebra) = struct
  open M

  type _ fix = In : 'a fix t -> 'a fix

  let unfix = function In a -> a
  let fix a = In a

  let rec cata (f : 'a t -> 'a) (muf : _ fix) =
    muf |> unfix |> fmap (cata f) |> f

  let rec ana (cof : 'a -> 'a t) (seed : 'a) =
    seed |> cof |> fmap (ana cof) |> fix

  let hylo (f : 'a t -> 'a) (cof : 'b -> 'b t) (seed : 'b) : 'a =
    seed |> ana cof |> cata f
end

module NatF = struct
  type _ t = Zero : 'a t | Succ : 'a -> 'a t

  let fmap f = function Zero -> Zero | Succ a -> Succ (f a)
end

module Nat = struct
  include Fix (NatF)
end

open NatF

let algebra = function Zero -> 0 | Succ i -> i + 1
let coalgebra i = if i == 0 then Zero else Succ (i - 1)

let%test "int" = Nat.cata algebra (Nat.ana coalgebra 10) == 10

module type A = sig
  type t
end

module ListF (Elem : A) = struct
  type _ t = Nil : 'a t | Cons : Elem.t * 'a -> 'a t

  let fmap f = function Nil -> Nil | Cons (a, b) -> Cons (a, f b)
end

(* module IntElem : ElemType with type t = int = struct *)
(* need to show the type explictly in the module signature *)
(* otherwise, it is abstract type *)
module IntElem = struct
  type t = int
end

module IntListF = ListF (IntElem)

module IntList = struct
  include Fix (IntListF)
end

open IntListF

let algebra_list = function Nil -> 0 | Cons (_, n) -> n + 1
let coalgebra_list = function [] -> Nil | x :: xs -> Cons (x, xs)
let length input = IntList.cata algebra_list input

let%test "list" =
  let list = [ 1; 2; 3 ] in
  let fix_list = IntList.ana coalgebra_list list in
  length fix_list == 3

module TreeF (Elem : A) = struct
  type _ t =
    | Leaf : 'a t
    | Singleton : Elem.t -> 'a t
    | Branch : 'a * 'a -> 'a t

  let fmap f = function
    | Leaf -> Leaf
    | Branch (l, r) -> Branch (f l, f r)
    | Singleton x -> Singleton x
end

module IntListTreeF = TreeF (struct
  type t = int list
end)

open IntListTreeF

let split = function
  | [] -> Leaf
  | [ x ] -> Singleton [ x ]
  | x ->
      let length = List.length x in
      let half = Int.div length 2 in
      let l, r = Base.List.split_n x half in
      Branch (l, r)

let rec merge_impl l r =
  match (l, r) with
  | [], _ -> r
  | _, [] -> l
  | x :: xs, y :: ys ->
      if x < y then x :: merge_impl xs r else y :: merge_impl l ys

let merge = function
  | Leaf -> []
  | Singleton a -> a
  | Branch (l, r) -> merge_impl l r

module Tree = struct
  include Fix (IntListTreeF)
end

let merge_sort x = Tree.hylo merge split x
let node () = Tree.In Leaf
let branch a b = Tree.In (Branch (a, b))

let%test_unit "rev" =
  [%test_eq: Base.int Base.list]
    (merge_sort [ 3; 2; 5; 4; 1 ])
    [ 1; 2; 3; 4; 5 ]
