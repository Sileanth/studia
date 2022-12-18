
type var = int 
type sym = int 
type rel = int
type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Neg
  | Imp of formula * formula
  | Rel of rel * term list
  | All of string * formula

module OrderedTerm : sig
  type t = term
  val compare : t -> t -> int
end = struct
  type t = term
  let rec compare a b = 
    match (a, b) with
    | Var a, Var b  -> if a < b then -1 else if a = b then 0 else 1
    | Sym _, Var _  -> 1
    | Var _, Sym _  -> -1
    | Sym (a, _) , Sym (b, _) when a < b -> -1 
    | Sym (a, _) , Sym (b, _) when a > b -> 1 
    | Sym (_, []) , Sym (_, []) -> 0
    | Sym (_, x :: xs) , Sym (_, [])  -> 1 
    | Sym (_, []) , Sym (_, x :: xs) -> -1
    | Sym (a, x :: xs) , Sym (b, y :: ys) -> match compare x y with
      | -1 -> -1
      | 1  -> 1
      | _  -> compare (Sym (a, xs)) (Sym (b, ys)) 
end
(* Moduł do generowania nowych nazw zmiennych by się nie dublowały. Chyba niepotrzebny *)
module NewVar : sig 
  val new_var : unit -> string
end = struct
  let count = ref 0
  let new_var () =
    let z = !count in
    count := !count + 1;
    string_of_int z
end
open NewVar
module VarMap = Map.Make(OrderedTerm)

let rec free_in_term (v : var) (t : term) = 
  match t with
  | Var nv      -> nv = v
  | Sym (_, ts) -> List.exists (free_in_term v) ts
and free_in_formula (v : var) ( f : formula) =
  match f with
  | Neg         -> false
  | All (_,  f) -> free_in_formula (v+1) f
  | Imp (p, s)  -> free_in_formula v p || free_in_formula v s
  | Rel (_, ts) -> List.exists (free_in_term v) ts

let rec apply_inc_in_term (inc : int) (t : term) : term =
  match t with 
  | Var v       -> Var (v+inc)
  | Sym (s, st) -> Sym (s, List.map (apply_inc_in_term inc) st)

let rec psubt_in_term_helper (inc : int) (map : term VarMap.t) (t : term) : term =
  match VarMap.find_opt (apply_inc_in_term (-inc) t) map with
  | Some sub -> apply_inc_in_term inc sub 
  | None     -> match t with
    | Var v        -> Var v
    | Sym (s , xt) -> Sym (s, List.map (psubt_in_term_helper inc map) xt)

let psubt_in_term = psubt_in_term_helper 0

let rec psub_in_formula_helper (inc : int) (map : term VarMap.t) (f : formula) : formula =
  match f with
  | Neg -> Neg
  | Imp (a, b) -> Imp (psub_in_formula_helper inc map a, psub_in_formula_helper inc map b)
  | Rel (s, ts) -> Rel (s, List.map (psubt_in_term_helper inc map) ts)
  | All (s, f) -> All (s, psub_in_formula_helper (inc + 1) map f)

let psub_in_formula = psub_in_formula_helper 0

let subst_in_term x s t = psubt_in_term (VarMap.singleton x s) t
let subst_in formula x s t = psub_in_formula (VarMap.singleton x s) t


type env = formula list
type theorem = env * formula


