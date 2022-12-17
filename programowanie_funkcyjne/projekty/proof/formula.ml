
type var = string
type sym = string
type rel = string

type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Var of var
  | Neg
  | Imp of formula * formula
  | Rel of rel * term list
  | All of var * formula


let rec free_in_term (v : var) (t : term) = 
  match t with
  | Var nv      -> nv = v
  | Sym (_, ts) -> List.exists (free_in_term v) ts
and free_in_formula (v : var) ( f : formula) =
  match f with
  | Var nv      -> nv = v
  | Neg         -> false
  | All (av, f) -> av <> v && free_in_formula v f
  | Imp (p, s)  -> free_in_formula v p || free_in_formula v s
  | Rel (_, ts) -> List.exists (free_in_term v) ts



