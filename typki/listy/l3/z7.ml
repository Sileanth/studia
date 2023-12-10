type var = string
module Var = String

let curr_id = ref 0
let new_id () = 
  curr_id := !curr_id + 1;
  !curr_id


type pretyp = 
  | PVar of var 
  | PArr of pretyp * pretyp 
  | PUnit
  | PMi of var * pretyp 



type typ =
  | TVar of var 
  | TArr of ind_typ * ind_typ 
  | TUnit
  | TMi of var * ind_typ 
and ind_typ = int * typ

let rec name_node = function 
  | PVar v -> (new_id (), TVar v)
  | PArr(l, r) -> (new_id (), TArr(name_node l, name_node r))
  | PUnit -> (new_id (), TUnit)
  | PMi(v, t) -> (new_id (), TMi(v, name_node t))

module Env = Map.Make(Var)
type envir = ind_typ Env.t
let empty_env = Env.empty

let max_size = 1000
let parent_arr = Array.init 1000 (fun x -> x) 
let rec find id = 
  let parent = Array.get parent_arr id in
  if  parent = id then id
  else begin 
    let leader = find parent in 
    Array.set parent_arr id leader; 
    leader
  end


let union id1 id2 = 
  let parent1 = find id1 in 
  let parent2 = find id2 in 
  Array.set parent_arr parent1 parent2

(*
  moze sie zapetlic na niepoprawnym typie 
  mi a. a
*)
let rec check_bisym it1 it2 env1 env2 =
  let (id1, t1) = it1 in 
  let (id2, t2) = it2 in
  match t1, t2 with 
    | TVar v, _                   -> check_bisym (Env.find v env1) it2 env1 env2
    | _, TVar v                   -> check_bisym it1 (Env.find v env2) env1 env2 
    | TMi(v, it), _               -> check_bisym it it2 (Env.add v it1 env1) env2 
    | _, TMi(v, it)               -> check_bisym it1 it env1 (Env.add v it2 env2) 
    | TUnit, TUnit                -> true 
    | TUnit, _                    -> false
    | _, TUnit                    -> false
    | TArr (l1, r1), TArr(l2, r2) -> if (find id1 = find id2) then true else (union id1 id2; check_bisym l1 l2 env1 env2 && check_bisym r1 r2 env1 env2)


let pre_term1 = PMi ("alpha", PArr(PArr(PUnit, PUnit) , PVar "alpha"))
let pre_term2 = PArr (PUnit, PMi ("alpha", PArr(PUnit, PVar "alpha")))
let term1 = name_node pre_term1 
let term2 = name_node pre_term2


let _ = 
  if check_bisym term1 term2 empty_env empty_env 
  then print_endline "smiga" 
  else print_endline "nie smiga"
