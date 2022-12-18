
type var = int 
type sym = string 
type rel = string 
type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Neg
  | Imp of formula * formula
  | Rel of rel * term list
  | All of string * formula

(* Pytanie otwarte czy chcę w All trzymać nazwę związanej zmiennej? *)

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
module VarMap = Map.Make(OrderedTerm)
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

let rec apply_inc_in_formula (inc : int) (f : formula) : formula=
  match f with 
  | Neg         -> Neg
  | Imp (a, b)  -> Imp (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | All (s, f)  -> All (s, apply_inc_in_formula (inc + 1) f)
  | Rel (s, ts) -> Rel (s, List.map (apply_inc_in_term inc) ts)

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
let subst_in_formula x s t = psub_in_formula (VarMap.singleton x s) t



let eq_term a b =
  a = b

let rec eq_formula a b = 
  match (a, b) with
  | All (_, a) , All (_, b)  -> eq_formula a b
  | Imp (a,b), Imp (c, d)    -> eq_formula a c && eq_formula b d
  | Neg, Neg                 -> true
  | Rel (x, xt), Rel (y, yt) -> x = y && xt = yt
  | _, _                     -> false

type env = formula list
type theorem = env * formula

let consequnce (_, cons) = cons
let assumptions (env, _) = env


let rec rem xs f = 
  match xs with
  | [] -> []
  | (x :: xs) when eq_formula x f -> xs
  | (x :: xs) -> x :: rem xs f


  (* Poprawić by używało eq_formula *)
let sum (xs : formula list) (ys : formula list)=
  xs @ List.filter (fun y -> not (List.exists (eq_formula y) xs)) ys




(* Reguły wnioskowania *)
let by_assumption f = 
  [f], f

let imp_i f (env, form) =
  rem env f, Imp (f, form)

let imp_e (env1, imp) (env2, pimp) =
  sum env1 env2, match imp with 
    | Imp (a, b) when eq_formula pimp a -> b
    | _  -> failwith "wrong usage of implication elimination"

let bot_e (env, f) f =
  if eq_formula f Neg then (env, f)
  else failwith "wrong usage of bottom elimination"

let all_i (env, f) =
  env, (All (new_var (), apply_inc_in_formula 1 f)) 


let all_e (env, all) t =
  env, match env with
    | All (_, f) -> subst_in_formula (Var 0) t (apply_inc_in_formula (-1) f)
    | _          -> failwith "wrong usage of for_all elimination"



