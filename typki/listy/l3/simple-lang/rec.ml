    
type t = 
| TUnit
| TEmpty
| TBool
| TInt 
| TArrow of t * t
| TPair of t * t
| TCoPair of t * t
| TUvar of uvar
and uvar_data = 
  | Typ of t
  | Constr of uvar list
and uvar = uvar_data ref

let t_unit = TUnit
let t_bool = TBool
let t_empty = TEmpty
let t_int = TInt
let t_arrow l r = TArrow(l, r)
let t_pair l r = TPair(l, r)
let c_copair l r = TCoPair(l, r)





let constrained_uvar cs = TUvar (ref (Constr cs)) 
let fresh_uvar () = constrained_uvar []


let rec contains_uvars xs = function
  | TUvar y -> List.exists (fun x -> y == x) xs
  | TUnit | TEmpty | TBool | TInt -> false
  | TArrow(t1, t2) | TPair(t1, t2) | TCoPair(t1, t2) ->
      contains_uvars xs t1 || contains_uvars xs t2

let rec unify_with_uvar x tp =
  match !x with
  | Typ t -> failwith "already unified"
  | Constr xs -> if contains_uvars (x :: xs) tp 
    then failwith "constrains not satisfied"
    else x := Typ tp

let rec unify tp1 tp2 =
  match tp1, tp2 with 
  | TUvar x, TUvar y when x == y -> ()
  | TUvar x, _ -> unify_with_uvar x tp1
  | _, TUvar y -> unify_with_uvar y tp2
  
  | TUnit, TUnit | TBool, TBool | TInt, TInt | TEmpty, TEmpty -> ()
  
  | TUnit, _ | _, TUnit 
  | TBool, _ | _, TBool
  | TInt, _ | _, TInt 
  | TEmpty, _ | _, TEmpty
  -> failwith "cannot unify"

  | TPair(l1, r1), TPair(l2, r2) 
  | TCoPair(l1, r1), TCoPair(l2, r2) 
  | TArrow(l1, r1), TArrow(l2, r2) 
  -> unify l1 l2; unify r1 r2;

  | TPair _, _ 
  | _, TPair _ 
  | TCoPair _, _ 
  | _, TCoPair _ 
  -> failwith "cannot unify"
  


type ast =
  | AUnit
  | ABool of bool
  | AInt of int
  | AFn of string * ast
  | AApp of ast * ast
  | APair of ast * ast
  | APiL of ast
  | APiR of ast
  | ALeft of ast
  | ARight of ast
  | ACase of ast * (string * ast) list
  | AMi of string * ast
  | ARecVar of string

type prenum_ast = 
  | AUnit
  | ABool of bool
  | AInt of int
  | AFn of string * numbered_ast
  | AApp of numbered_ast * numbered_ast
  | APair of numbered_ast * numbered_ast
  | APiL of numbered_ast
  | APiR of numbered_ast
  | ALeft of numbered_ast
  | ARight of numbered_ast
  | ACase of ast * (string * numbered_ast) list
  | AMi of string * numbered_ast
  | ARecVar of string
and numbered_ast = int * prenum_ast

module Env = Map.Make(String)

type env = uvar Env.t




