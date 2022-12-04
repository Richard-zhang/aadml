type _ t = Leaf : 'a t | Branch : ('a * 'a t * 'a t) -> 'a t
type 'a tree = N of 'a * 'a tree * 'a tree | L

let rec fold_tree (f : 'a -> 'b -> 'b -> 'b) (init : 'b) = function
  | L -> init
  | N (v, left_t, right_t) -> f v (fold_tree f init left_t) (fold_tree f init right_t)


let rec fold (f : 'a -> 'b -> 'b -> 'b) (init : 'b) = function
  | Leaf -> init
  | Branch (v, left, right) -> f v (fold f init left) (fold f init right)

let map_t (f : 'a -> 'b) = fold (fun v l r -> Branch (f v, l, r)) Leaf
let depth t = fold (fun _ l r -> 1 + max l r) 0 t

let depth_label t =
  fold
    (fun _ fdl fdr i -> Branch (i, fdl (i + 1), fdr (i + 1)))
    (fun _ -> Leaf)
    t

let fold_cps (f : 'a -> 'b -> 'b -> 'b) (init : 'b) t =
  let rec fold_helper t cont =
    match t with
    | Leaf -> cont init
    | Branch (v, l, r) ->
        fold_helper l (fun vl -> fold_helper r (fun vr -> f v vl vr |> cont))
  in
  fold_helper t Base.Fn.id
