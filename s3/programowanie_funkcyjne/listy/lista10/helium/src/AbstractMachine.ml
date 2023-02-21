open Lang.Node
open Lang.Untyped

exception Runtime_error

type nat =
| Z
| S of nat

type eff_id = Utils.UID.t

type eff_value =
| EVPure
| EVId   of eff_id
| EVSet  of nat Utils.UID.Map.t

type coercion =
| CLift of eff_value
| CSwap of eff_value * eff_value
| CCons of eff_value * coercion
| CComp of coercion * coercion

type env = value Var.Map.t

and closure = (var * env Lazy.t * expr)

and value =
| VNum      of int
| VChar     of char
| VString   of string
| VClo      of closure
| VCont     of mcont
| VCtor     of int * value list
| VPrim     of (value -> value)
| VEffect   of eff_value
| VCoercion of coercion
| VSeal     of Utils.Seal.seal

and cont =
| CDone
| CApp1     of env * expr * cont
| CApp2     of value * cont
| COp       of eff_id * int * value list * env * expr list * cont
| CCtor     of int * value list * env * expr list * cont
| CLet      of var * env * expr * cont
| CMatch    of env * clause list * cont
| CCoerce   of env * expr * cont
| CEffCons1 of env * expr * cont
| CEffCons2 of eff_value * cont
| CMkLift   of cont
| CMkSwap1  of env * expr * cont
| CMkSwap2  of eff_value * cont
| CMkCons1  of env * expr * cont
| CMkCons2  of eff_value * cont
| CMkComp1  of env * expr * cont
| CMkComp2  of coercion * cont
| CReplExpr of Utils.Seal.t * env * (unit -> expr) * cont

and mcont = (mframe * cont) list

and mframe =
| MHandle of eff_id * env * var * expr * handler list
| MCoerce of coercion

type coerce_frame =
| CF_Cons   of nat
| CF_Coerce of coercion

let pretty_flow_node = Flow.Node.create "AM value"
let flow_node = Flow.Node.create "AM value"

let value_class =
  let open Predef.Value in
  { v_unit    = VCtor(0, [])
  ; list_nil  = VCtor(0, [])
  ; list_cons = (fun x xs -> VCtor(1, [x; xs]))
  ; of_int    = (fun n -> VNum n)
  ; of_bool   = (fun b -> VCtor((if b then 1 else 0), []))
  ; of_string = (fun s -> VString s)
  ; of_char   = (fun c -> VChar c)
  ; of_func   = (fun f -> VPrim f)
  ; of_seal   = (fun s -> VSeal s)
  ; to_int    =
    begin function
    | VNum n -> n
    | _ -> failwith "AM: not an integer"
    end
  ; to_bool   =
    begin function
    | VCtor(0, []) -> false
    | VCtor(1, []) -> true
    | _ -> failwith "AM: not a boolean"
    end
  ; to_string =
    begin function
    | VString s -> s
    | _ -> failwith "AM: not a string"
    end
  ; to_char =
    begin function
    | VChar c -> c
    | _ -> failwith "AM: not a char"
    end
  ; to_seal =
    begin function
    | VSeal s -> s
    | _ -> failwith "AM: not a seal"
    end
  }

module Nat = struct
  let rec add n m =
    match n with
    | Z   -> m
    | S n -> S (add n m)

  let rec sub n m =
    match m with
    | Z   -> Some n
    | S m ->
      begin match n with
      | Z   -> None
      | S n -> sub n m
      end
end

module EffValue = struct
  let count s l =
    match s with
    | EVPure  -> Z
    | EVId l' -> if Utils.UID.equal l l' then S Z else Z
    | EVSet s ->
      begin match Utils.UID.Map.find_opt l s with
      | None   -> Z
      | Some n -> n
      end

  let lift s l n =
    Nat.add (count s l) n

  let swap s1 s2 l n =
    match Nat.sub n (count s1 l) with
    | None    -> Nat.add (count s2 l) n
    | Some n1 ->
      begin match Nat.sub n1 (count s2 l) with
      | None   -> n1
      | Some _ -> n
      end

  let to_set eff =
    match eff with
    | EVPure  -> Utils.UID.Map.empty
    | EVId l  -> Utils.UID.Map.singleton l (S Z)
    | EVSet s -> s

  let join eff1 eff2 =
    match eff1, eff2 with
    | EVPure, eff2 -> eff2
    | eff1, EVPure -> eff1
    | eff1, eff2   ->
      EVSet (Utils.UID.Map.merge (fun _ n1 n2 ->
          match n1, n2 with
          | None, n2 -> n2
          | n1, None -> n1
          | Some n1, Some n2 -> Some (Nat.add n1 n2)
        ) (to_set eff1) (to_set eff2))
end

module Env = struct
  let empty = Var.Map.empty

  let lookup env x = Var.Map.find x env
  
  let lookup_eff env l = 
    match Var.Map.find l env with
    | VEffect (EVId l) -> l
    | _                -> raise Runtime_error

  let add env x v = Var.Map.add x v env
  let add_vals env xs vs = List.fold_left2 add env xs vs

  let new_effect env l =
    Var.Map.add l (VEffect (EVId (Utils.UID.fresh ()))) env

  let fix env rfs =
    let env_ref = ref env in
    let env = List.fold_left (fun env (x, y, body) ->
        add env x (VClo(y, lazy (!env_ref), body))
      ) env rfs in
    env_ref := env;
    env
end

let rec eval e env c stack =
  match e.data with
  | EEffPure          -> cont c (VEffect EVPure) stack
  | EEffCons(e1, e2)  -> eval e1 env (CEffCons1(env, e2, c)) stack
  | ENum n            -> cont c (VNum n) stack
  | EChar ch          -> cont c (VChar ch) stack
  | EString s         -> cont c (VString s) stack
  | EVar x            -> cont c (Env.lookup env x) stack
  | EFun(x, e)        -> cont c (VClo(x, lazy env, e)) stack
  | EOp(l, n,[])      -> grab (Env.lookup_eff env l) Z c stack n [] []
  | EOp(l, n, e :: es) ->
    eval e env (COp(Env.lookup_eff env l, n, [], env, es, c)) stack
  | ECtor(n, [])      -> cont c (VCtor(n, [])) stack
  | ECtor(n, e :: es) -> eval e env (CCtor(n, [], env, es, c)) stack
  | EApp(e1, e2)      -> eval e1 env (CApp1(env, e2, c)) stack
  | ELet(x, e1, e2)   -> eval e1 env (CLet(x, env, e2, c)) stack
  | EFix(rfs, e)      -> eval e (Env.fix env rfs) c stack
  | EHandle(l, e, x, r, hs) ->
    eval e env CDone
      ((MHandle(Env.lookup_eff env l, env, x, r, hs), c) :: stack)
  | EMatch(e, cls)   -> eval e env (CMatch(env, cls, c)) stack
  | ENewEffect(l, e) -> eval e (Env.new_effect env l) c stack
  | ECoerce(e1, e2)  -> eval e1 env (CCoerce(env, e2, c)) stack
  | ECLift e         -> eval e env (CMkLift c) stack
  | ECSwap(e1, e2)   -> eval e1 env (CMkSwap1(env, e2, c)) stack
  | ECCons(e1, e2)   -> eval e1 env (CMkCons1(env, e2, c)) stack
  | ECComp(e1, e2)   -> eval e1 env (CMkComp1(env, e2, c)) stack
  | EExtern name     ->
    let v = (Predef.DB.get_extern name).Predef.Value.mk_value value_class in
    cont c v stack
  | EReplExpr(e, seal, repl) ->
    eval e env (CReplExpr(seal, env, repl, c)) stack
  | ERepl(seal, repl) -> eval_repl seal env repl c stack

and cont c v stack =
  match c with
  | CDone                     -> mcont stack v
  | CApp1(env, e, c)          -> eval e env (CApp2(v, c)) stack
  | CApp2(VClo(x, lazy env, e), c) ->
    eval e (Env.add env x v) c stack
  | CApp2(VCont k, c)         -> resume k v c stack
  | CApp2(VPrim f, c)         -> cont c (f v) stack
  | CApp2((VNum _ | VCtor _ | VEffect _ | VChar _ | VString _ | VSeal _ | VCoercion _), _) ->
    raise Runtime_error
  | COp(l, n, vs, _, [], c)   -> grab l Z c stack n (List.rev (v :: vs)) []
  | COp(l, n, vs, env, e :: es, c) ->
    eval e env (COp(l, n, v :: vs, env, es, c)) stack
  | CCtor(n, vs, _, [], c)    -> cont c (VCtor(n, List.rev (v :: vs))) stack
  | CCtor(n, vs, env, e :: es, c) ->
    eval e env (CCtor(n, v :: vs, env, es, c)) stack
  | CLet(x, env, e, c)        -> eval e (Env.add env x v) c stack
  | CMatch(env, cls, c)       ->
    begin match v with
    | VCtor(n, vs) ->
      let (xs, e) = List.nth cls n in
      let env = Env.add_vals env xs vs in
      eval e env c stack
    | _ -> raise Runtime_error
    end
  | CCoerce(env, e, c) ->
    begin match v with
    | VCoercion cc -> eval e env CDone ((MCoerce cc, c) :: stack)
    | _ -> raise Runtime_error
    end
  | CEffCons1(env, e, c) ->
    begin match v with
    | VEffect eff -> eval e env (CEffCons2(eff, c)) stack
    | _ -> raise Runtime_error
    end
  | CEffCons2(eff1, c) ->
    begin match v with
    | VEffect eff2 -> cont c (VEffect(EffValue.join eff1 eff2)) stack
    | _ -> raise Runtime_error
    end
  | CMkLift c ->
    begin match v with
    | VEffect eff -> cont c (VCoercion (CLift eff)) stack
    | _ -> raise Runtime_error
    end
  | CMkSwap1(env, e, c) ->
    begin match v with
    | VEffect eff -> eval e env (CMkSwap2(eff, c)) stack
    | _ -> raise Runtime_error
    end
  | CMkSwap2(eff1, c) ->
    begin match v with
    | VEffect eff2 -> cont c (VCoercion(CSwap(eff1, eff2))) stack
    | _ -> raise Runtime_error
    end
  | CMkCons1(env, e, c) ->
    begin match v with
    | VEffect eff -> eval e env (CMkCons2(eff, c)) stack
    | _ -> raise Runtime_error
    end
  | CMkCons2(eff, c) ->
    begin match v with
    | VCoercion cc -> cont c (VCoercion(CCons(eff, cc))) stack
    | _ -> raise Runtime_error
    end
  | CMkComp1(env, e, c) ->
    begin match v with
    | VCoercion cc -> eval e env (CMkComp2(cc, c)) stack
    | _ -> raise Runtime_error
    end
  | CMkComp2(cc1, c) ->
    begin match v with
    | VCoercion cc2 -> cont c (VCoercion(CComp(cc1, cc2))) stack
    | _ -> raise Runtime_error
    end
  | CReplExpr(seal, env, repl, c) ->
    Settings.repl_print_value seal (value_class.Predef.Value.to_string v);
    eval_repl seal env repl c stack

and mcont stack v =
  match stack with
  | [] -> v
  | (MHandle(_, env, x, ret, _), c) :: stack ->
    eval ret (Env.add env x v) c stack
  | (MCoerce _, c) :: stack ->
    cont c v stack

and grab l n c stack op vs k =
  match stack with
  | []         -> raise Runtime_error
  | (m, c') :: stack ->
    begin match m with
    | MHandle(l', env, _, _, hs) when Utils.UID.equal l l' ->
      begin match n with
      | Z ->
        let (xs, r, e) = List.nth hs op in
        let env = Env.add (Env.add_vals env xs vs) r (VCont((m, c) :: k)) in
        eval e env c' stack
      | S n ->
        grab l n c' stack op vs ((m, c) :: k)
      end
    | MHandle _  -> grab l n c' stack op vs ((m, c) :: k)
    | MCoerce cc ->
      coerce cc [] l n (c', stack, op, vs, ((m, c) :: k))
    end

and coerce cc ccs l n rest =
  match cc with
  | CLift s ->
    coerce_cont ccs l (EffValue.lift s l n) rest
  | CSwap(s1, s2) ->
    coerce_cont ccs l (EffValue.swap s1 s2 l n) rest
  | CCons(s, cc) ->
    begin match EffValue.count s l with
    | Z -> coerce cc ccs l n rest
    | m ->
      begin match Nat.sub n m with
      | None   -> coerce_cont ccs l n rest
      | Some n -> coerce cc (CF_Cons m :: ccs) l n rest
      end
    end
  | CComp(cc1, cc2) ->
    coerce cc1 (CF_Coerce cc2 :: ccs) l n rest

and coerce_cont ccs l n rest =
  match ccs with
  | [] ->
    let (c, stack, op, v, k) = rest in
    grab l n c stack op v k
  | CF_Cons m    :: ccs -> coerce_cont ccs l (Nat.add m n) rest
  | CF_Coerce cc :: ccs -> coerce cc ccs l n rest

and resume k v c stack =
  match k with
  | []           -> cont c v stack
  | (m, c') :: k -> resume k v c' ((m, c) :: stack)

and eval_repl seal env repl c stack =
  eval (Settings.repl seal repl) env c stack

let init_env () =
  let open Predef.DB in
  List.fold_left (fun env (Type p) ->
      match p.untyped_var with
      | Some x -> Env.add env x (VEffect EVPure)
      | None   -> env
  ) Env.empty (types ())

let run e = eval e (init_env ()) CDone []

let eval_tag = Flow.Tag.create
  ~cmd_flag:  "-eval-am"
  ~cmd_descr: " Evaluate a program using abstract machine."
  "AM_eval"

let _ =
  Flow.register_transform
    ~contracts: [ eval_tag; CommonTags.eval ]
    ~source: Lang.Untyped.flow_node
    ~target: flow_node
    run
