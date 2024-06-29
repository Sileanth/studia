type var = string 

module M = Map.Make(String)


type typ = 
  | TUnit 
  | TArrow of typ * typ 
  | TPair of typ * typ 
  | TSum of typ * typ 
  | TNum 
  | TBool


type typ_env = typ M.t

type expr = 
  | EVar of var
  | EApp of expr * expr 
  | ELam of typ * var * expr
  | EUnit 
  | EBool of bool 
  | ENum of int 
  | EPair of expr * expr 
  | EFst of expr 
  | ESnd of expr 
  | ELeft of expr * typ 
  | ERight of typ * expr 
  | EMatch of expr * var * expr * var * expr
  | EIf of expr * expr * expr 
  | EEq of expr * expr 
  | EPlus of expr * expr 
  | EMin of expr * expr
  | EError of typ * string




type value = 
  | VUnit 
  | VBool of bool
  | VNum of int
  | VLam of env * var * expr 
  | VPair of value * value 
  | VLeft of value 
  | VRight of value 
and env = value M.t


let rec eval (env: env) e = 
  match e with 
  | EUnit -> VUnit
  | EBool b -> VBool b 
  | ENum n -> VNum n
  | ELam (t, v, e) -> VLam (env, v, e)
  | EVar v -> M.find v env 
  | EPair (e1, e2) -> VPair (eval env e1, eval env e2) 
  | ELeft (e, t) -> VLeft (eval env e)
  | ERight (t, e) -> VRight (eval env e)
  | EApp (e1, e2) -> begin match eval env e1 with
    | VLam (lam_env, var, body) -> eval (M.add var (eval env e2) lam_env) body
    | _ -> failwith "application expected lambda"
    end
  | EFst ep -> begin match eval env ep with 
    | VPair (v1, _) -> v1 
    | _ -> failwith "fst expected pair"
    end 
  | ESnd ep -> begin match eval env ep with 
    | VPair (_, v2) -> v2 
    | _ -> failwith "snd expected pair"
    end 
  | EMatch (e, vl, el, vr, er) -> begin match eval env e with 
    | VLeft v -> eval (M.add vl v env) el 
    | VRight v -> eval (M.add vr v env) er 
    | _ -> failwith "match expected Left or Right value"
    end  
  | EPlus (e1, e2) -> begin match (eval env e1, eval env e2) with
    | (VNum n1, VNum n2) -> VNum (n1 + n2)
    | _ -> failwith "plus expected numbers"
    end
  | EMin (e1, e2) -> begin match (eval env e1, eval env e2) with
    | (VNum n1, VNum n2) -> VNum (n1 - n2)
    | _ -> failwith "minus expected numbers"
    end
  | EEq (e1, e2) -> begin match (eval env e1, eval env e2) with
    | (VNum n1, VNum n2) -> VBool (n1 = n2)
    | _ -> failwith "equality expected numbers"
    end
  | EIf (cond, e2, e3) -> begin match (eval env cond, eval env e2, eval env e3) with 
    | (VBool b, v1, v2) -> if b then v1 else v2 
    | _ -> failwith "if expected bool"
    end
  | EError (t,s) -> failwith s


let rec type_check tenv e = 
  match e with 
  | EVar v -> M.find v tenv
  | EUnit -> TUnit 
  | ENum n -> TNum 
  | EBool b -> TBool
  | ELam (t, v, e) -> TArrow (t, type_check (M.add v t tenv) e)
  | EPair (e1, e2) -> TPair (type_check tenv e1, type_check tenv e2)
  | ELeft (e, t) -> TSum (type_check tenv e, t) 
  | ERight (t, e) -> TSum (t, type_check tenv e)
  | EFst e -> begin match type_check tenv e with
    | TPair (t1, t2) -> t1 
    | _ -> failwith "fst typing error"
    end
  | ESnd e -> begin match type_check tenv e with
    | TPair (t1, t2) -> t2 
    | _ -> failwith "snd typing error"
    end
  | EMatch (e, vl, el, vr, er) -> begin match type_check tenv e with 
    | TSum (tl, tr) -> begin match (type_check (M.add vl tl tenv) el, type_check (M.add vr tr tenv) er) with 
      | (tb1, tb2) when tb1 = tb2 -> tb1 
      | _ -> failwith "match branches has diffrent types"
      end
    | _ -> failwith "match expected sum type" 
    end 
  | EApp (e1, e2) -> begin match (type_check tenv e1, type_check tenv e2) with 
    | (TArrow (t1, t2), targ) when t1 = targ -> t2 
    | _ -> failwith "application typing error"
    end
  | EPlus (e1, e2) -> begin match (type_check tenv e1, type_check tenv e2) with 
    | (TNum, TNum) -> TNum 
    | _ -> failwith "plus work in int types"
    end
  | EMin (e1, e2) -> begin match (type_check tenv e1, type_check tenv e2) with 
    | (TNum, TNum) -> TNum 
    | _ -> failwith "plus work in int types"
    end
  | EEq (e1, e2) -> begin match (type_check tenv e1, type_check tenv e2) with 
    | (TNum, TNum) -> TBool 
    | _ -> failwith "plus work in int types"
    end
  | EIf (cond, e1, e2) -> begin match (type_check tenv cond, type_check tenv e1, type_check tenv e2) with 
    | (TBool, t1, t2) when t1 = t2 -> t1 
    | _ -> failwith "if expected bool, or difrrent types in branches"
    end
  | EError (t, s) -> t
  


 

