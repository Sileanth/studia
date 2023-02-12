
type var = int 
type rel_var = int
type sym = string 
type rel = string 
type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Neg
  | Top
  | Imp of formula * formula
  | And of formula * formula
  | Or  of formula * formula
  | Rel of rel * term list
  | All of string * formula
  | Ex  of string * formula




module OrderedVar: sig
  type t = var 
  val compare : t -> t -> int
end = struct
  type t = var
  let compare a b =
    if a = b then 0
    else if a < b then -1
    else 1
end
module VarMap = Map.Make(OrderedVar)

let rec string_of_term (map : string VarMap.t) (t : term) : string =
        match t with
        | Var x       -> VarMap.find x map
        | Sym (s, ts) -> 
                        if ts <> [] then 
                                let string_terms_list = List.map (string_of_term map) ts in
                                let string_terms = List.fold_right (fun t a -> t ^ ", " ^ a) string_terms_list ""
                                in let cleaned_string = String.sub string_terms 0 (String.length string_terms - 2)
                                in s ^ "(" ^ cleaned_string ^ ")"
                        else
                                s ^ "()"


let rec string_of_formula (map : string VarMap.t) (f : formula) : string = 
        let bin name a b =
                "(" ^ string_of_formula map a ^ " " ^ name ^ " " ^ string_of_formula map b ^ ")" 
        in let kwant sym name f =
                let increased_map = map |> VarMap.to_seq |> Seq.map (fun (k, v) -> (k+1, v)) |> VarMap.of_seq
                in let map_with_new_key = VarMap.add 0 name increased_map 
                in sym ^ name ^ "." ^ string_of_formula map_with_new_key f
        in let rel name ts = 
                string_of_term map (Sym (name, ts)) 
        in match f with 
        | Neg            -> "⊥"
        | Top            -> "⊤"
        | Imp (a, b)     -> bin "->" a b 
        | Or (a, b)      -> bin "∨" a b
        | And (a, b)     -> bin "∧" a b
        | Rel (name, ts) -> rel name ts
        | All (name, f)  -> kwant "∀" name f
        | Ex (name, f)   -> kwant "∃" name f


let rec max_free_var_in_term (t : term) =
  match t with
  | Var v  -> v
  | Sym (s, st) -> List.fold_left max (0) (List.map max_free_var_in_term st)

let rec max_free_var_in_formula (f : formula) = 
  let bin a b =
    max (max_free_var_in_formula a) (max_free_var_in_formula b) 
  in let kwant f =
    max ((max_free_var_in_formula f) - 1) 0
  in match f with
  | Neg                 -> 0
  | Top                 -> 0
  | Imp (a, b)          -> bin a b
  | And (a, b)          -> bin a b
  | Or  (a, b)          -> bin a b 
  | Rel (_, st)         -> List.fold_left max (0) (List.map max_free_var_in_term st)
  | All (_, f)          -> kwant f 
  | Ex  (_, f)          -> kwant f
 

let gen_free_var_in_term t =
  max_free_var_in_term t + 1

let gen_free_var_in_formula t =
  max_free_var_in_formula t + 1


let rec free_in_term (v : var) (t : term) = 
  match t with
  | Var nv      -> nv = v
  | Sym (_, ts) -> List.exists (free_in_term v) ts 
and free_in_formula (v : var) ( f : formula) =
  let zero =
    false
  in let bin a b = 
    free_in_formula v a || free_in_formula v b
  in let kwant f = 
    free_in_formula (v+1) f
  in match f with
  | Neg         -> zero
  | Top         -> zero 
  | All (_,  f) -> kwant f 
  | Ex  (_,  f) -> kwant f 
  | Imp (p, s)  -> bin p s 
  | And (p, s)  -> bin p s 
  | Or  (p, s)  -> bin p s 
  | Rel (_, ts) -> List.exists (free_in_term v) ts 

let rec apply_inc_in_term (inc : int) (t : term) : term =
  match t with 
  | Var v       -> Var (v+inc)
  | Sym (s, st) -> Sym (s, List.map (apply_inc_in_term inc) st)

let rec apply_inc_in_formula (inc : int) (f : formula) : formula=
  match f with 
  | Neg         -> Neg
  | Top         -> Top 
  | Imp (a, b)  -> Imp (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | And (a, b)  -> And (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | Or  (a, b)  -> Or  (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | All (s, f)  -> All (s, apply_inc_in_formula inc f)
  | Ex  (s, f)  -> Ex (s, apply_inc_in_formula inc f)
  | Rel (s, ts) -> Rel (s, List.map (apply_inc_in_term inc) ts)

let rec psubt_in_term_helper (inc : int) (map : term VarMap.t) (t : term) : term =
    match t with
    | Var v -> begin match (VarMap.find_opt (v - inc) map) with
      | None   -> Var v
      | Some t -> apply_inc_in_term inc t 
    end
    | Sym (s , xt) -> Sym (s, List.map (psubt_in_term_helper inc map) xt)

let psubt_in_term = psubt_in_term_helper 0

let rec psub_in_formula_helper (inc : int) (map : term VarMap.t) (f : formula) : formula =
  let sing f =
    psub_in_formula_helper inc map f
  in let kwant f =
    psub_in_formula_helper (inc + 1) map f
    in match f with
  | Neg         -> Neg
  | Top         -> Top 
  | Imp (a, b)  -> Imp (sing a, sing b) 
  | And (a, b)  -> And (sing a, sing b) 
  | Or  (a, b)  -> Or  (sing a, sing b) 
  | Rel (s, ts) -> Rel (s, List.map (psubt_in_term_helper inc map) ts)
  | All (s, f)  -> All (s, kwant f)   
  | Ex  (s, f)  -> Ex (s, kwant f) 

let psub_in_formula = psub_in_formula_helper 0

let subst_in_term x s t = psubt_in_term (VarMap.singleton x s) t
let subst_in_formula x s f = psub_in_formula (VarMap.singleton x s) f



let eq_term a b =
  a = b

let rec eq_formula a b = 
  let kwant a b =
    eq_formula a b
  in let bin a b c d =
    eq_formula a c && eq_formula b d
  in match (a, b) with
  | All (_, a) , All (_, b)  -> kwant a b 
  | Ex  (_, a) , Ex (_, b)   -> kwant a b 
  | Imp (a,b), Imp (c, d)    -> bin a b c d 
  | And (a,b), And (c, d)    -> bin a b c d 
  | Or (a,b), Or (c, d)      -> bin a b c d 
  | Neg, Neg                 -> true
  | Top, Top                 -> true
  | Rel (x, xt), Rel (y, yt) -> x = y && xt = yt
  | _, _                     -> false
