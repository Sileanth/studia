
type var = string 
type sym = string 
type rel = string 

type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Neg
  | Imp of formula * formula
  | Rel of rel * term list
  | All of var * formula

module OrderedTerm : sig
  type t = term
  val compare : t -> t -> int
end = struct
  type t = term
  (* do poprawki ta funkcja *)
  let rec compare a b = 
    match (a, b) with
    | Var a, Var b  -> if a < b then -1 else if a = b then 0 else 1
    | Sym _, Var _  -> 1
    | Var _, Sym _  -> -1
    | Sym _ , Sym _ -> 0 
end
(* Moduł do generowania nowych nazw zmiennych by się nie dublowały *)
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
  (* | Var nv      -> nv = v *)
  | Neg         -> false
  | All (av,  f) -> av <> v && free_in_formula v f
  | Imp (p, s)  -> free_in_formula v p || free_in_formula v s
  | Rel (_, ts) -> List.exists (free_in_term v) ts

let rec psubt_in_term (map : term VarMap.t) (t : term) : term =
  match VarMap.find_opt t map with
  | Some sub -> sub
  | None     -> match t with
    | Var v        -> Var v
    | Sym (s , xt) -> Sym (s, List.map (psubt_in_term map) xt)

let rec psubt_in_formula (map : term VarMap.t) (f : formula) : formula =
  match f with
  | Neg         -> Neg
  | Imp (a, b)  -> Imp (psubt_in_formula map a, psubt_in_formula map b)
  | Rel (s, ts) -> Rel (s, List.map (psubt_in_term map) ts)
  | All (v, f)  -> match VarMap.find_opt (Var v) map with
    | Some _ -> psubt_in_formula (VarMap.remove (Var v) map) (All (v, f)) 
    | None   -> match VarMap.for_all (fun _ st -> free_in_term v st) map with
      | true  -> All (v, psubt_in_formula map f)
      | false -> let z = new_var () in
        All (z, psubt_in_formula (VarMap.add (Var v) (Var z) map) f) 

let subst_in_term x s t = psubt_in_term (VarMap.singleton x s) t
let subst_in_formula x s f = psubt_in_formula (VarMap.singleton x s) f

type env = formula list
type theorem = env * formula


