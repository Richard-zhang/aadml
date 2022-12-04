type _ t = Int : int t | B : float t | Other : 'a t
type any_t = Any_t : 'a t -> any_t

let deserial : string -> any_t = function
  | "Int" -> Any_t Int
  | "B" -> Any_t B
  | _ -> Any_t Other

type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int v -> v
  | Add -> ( + )
  | App (f, x) -> eval f @@ eval x

type _ ref_t = Float : float ref_t | String : string ref_t

let deep (None : char ref_t option) : char = 'a'

(* mutual recursion *)
type _ closure = Closure : ('a -> 'b) * 'a -> 'b closure

(* let eval_closure (Closure (type a) ((f, x) : (a -> _) * a)) = f x *)
let eval_closure (Closure (f, a)) = f a

type _ ext_t = ..
type _ ext_t += Id : int ext_t
