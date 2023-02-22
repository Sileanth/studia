open Lang.Node
open Lang.UCPS

exception Runtime_error

module EffectLabel : sig
  type t
  val fresh : unit -> t
  val equal : t -> t -> bool
  module Map : Map.S with type key = t
end = Utils.UID

module Effect : sig
  type t

  val pure : t
  val id   : EffectLabel.t -> t
  val cons : t -> t -> t

  val is_pure : t -> bool

  val count : EffectLabel.t -> t -> int
end = struct
  type t = int EffectLabel.Map.t

  let pure = EffectLabel.Map.empty
  let id l = EffectLabel.Map.singleton l 1
  
  let cons =
    EffectLabel.Map.merge (fun _ m1 m2 ->
      match m1, m2 with
      | None, m | m, None -> m
      | Some n1, Some n2 -> Some (n1 + n2))

  let is_pure = EffectLabel.Map.is_empty

  let count l e =
    match EffectLabel.Map.find_opt l e with
    | None   -> 0
    | Some n -> n
end

type value =
| VNum    of int
| VChar   of char
| VString of string
| VFun    of (value -> value)
| VCtor   of int * value list
| VEffect of Effect.t
| VSeal   of Utils.Seal.seal

let apply v1 v2 =
  match v1 with
  | VFun f -> f v2
  | _      -> raise Runtime_error

let to_effect v =
  match v with
  | VEffect e -> e
  | _ -> raise Runtime_error

module Env : sig
  type t

  val empty : t

  val extend       : t -> var -> value -> t
  val extend_l     : t -> var list -> value list -> t
  val fresh_effect : t -> lvar -> t

  val lookup_var  : t -> var  -> value
  val lookup_lvar : t -> lvar -> EffectLabel.t
end = struct
  type t =
    { var_env  : value Var.Map.t
    ; lvar_env : EffectLabel.t LVar.Map.t
    }

  let empty =
    { var_env  = Var.Map.empty
    ; lvar_env = LVar.Map.empty
    }

  let extend env x v =
    { env with
      var_env = Var.Map.add x v env.var_env
    }

  let rec extend_l env xs vs =
    match xs, vs with
    | [], [] -> env
    | x :: xs, v :: vs -> extend_l (extend env x v) xs vs
    | _ -> raise Runtime_error

  let fresh_effect env l =
    { env with
      lvar_env = LVar.Map.add l (EffectLabel.fresh ()) env.lvar_env
    }

  let lookup_var env x =
    Var.Map.find x env.var_env

  let lookup_lvar env l =
    LVar.Map.find l env.lvar_env
end

module Coercion : sig
  type t

  val id   : t
  val lift : Effect.t -> t
  val swap : Effect.t -> Effect.t -> t
  val cons : Effect.t -> t -> t
  val comp : t -> t -> t

  val coerce : t -> EffectLabel.t -> int -> int
end = struct
  type t =
  | Id
  | Lift of Effect.t
  | Swap of Effect.t * Effect.t
  | Cons of Effect.t * t
  | Comp of t * t

  let id         = Id
  let lift e     = Lift e
  let swap e1 e2 = Swap(e1, e2)
  let cons e  c  = Cons(e, c)
  let comp c1 c2 = Comp(c1, c2)

  let rec coerce c l n =
    match c with
    | Id           -> n
    | Lift e       -> n + Effect.count l e
    | Swap(e1, e2) ->
      let n1 = Effect.count l e1 in
      let n2 = Effect.count l e2 in
      if n < n1 then n + n2
      else if n < n1 + n2 then n - n1
      else n
    | Cons(e, c) ->
      let n1 = Effect.count l e in
      if n < n1 then n
      else n1 + coerce c l (n - n1)
    | Comp(c1, c2) -> coerce c2 l (coerce c1 l n)
end

type frame =
| FPureHandle of
    { label     : EffectLabel.t
    ; return_cl : (value -> value)
    ; handlers  : (value list -> value -> value) list
    }
| FHandle of
    { label     : EffectLabel.t
    ; return_cl : (value -> value -> value)
    ; handlers  : (value list -> value -> value -> value) list
    ; cont      : value
    }
| FCoerce of Coercion.t * value

let value_class =
  let open Predef.Value in
  { v_unit    = VCtor(0, [])
  ; list_nil  = VCtor(0, [])
  ; list_cons = (fun x xs -> VCtor(1, [x; xs]))
  ; of_int    = (fun n -> VNum n)
  ; of_bool   = (fun b -> VCtor((if b then 1 else 0), []))
  ; of_string = (fun s -> VString s)
  ; of_char   = (fun c -> VChar c)
  ; of_func   = (fun f -> VFun f)
  ; of_seal   = (fun s -> VSeal s)
  ; to_int    =
    begin function
    | VNum n -> n
    | _ -> failwith "UCPS: not an integer"
    end
  ; to_bool   =
    begin function
    | VCtor(0, []) -> false
    | VCtor(1, []) -> true
    | _ -> failwith "UCPS: not a boolean"
    end
  ; to_string =
    begin function
    | VString s -> s
    | _ -> failwith "UCPS: not a string"
    end
  ; to_char =
    begin function
    | VChar c -> c
    | _ -> failwith "UCPS: not a char"
    end
  ; to_seal =
    begin function
    | VSeal s -> s
    | _ -> failwith "UCPS: not a seal"
    end
  }

let run expr =
  let stack = ref [] in

  let push_frame frame =
    stack := frame :: !stack
  in

  let pop_frame () =
    match !stack with
    | [] -> None
    | frame :: st ->
      stack := st;
      Some frame
  in

  let grab l n frames i args cont =
    let rec grab n frames =
      match pop_frame () with
      | None -> raise Runtime_error
      | Some frame ->
        begin match frame with
        | FPureHandle fh ->
          (* Pure handlers have to handle all effects *)
          if not (EffectLabel.equal l fh.label) || n > 0 then
            raise Runtime_error
          else
            begin match List.nth_opt fh.handlers i with
            | None   -> raise Runtime_error
            | Some h ->
              let res = VFun(fun arg ->
                push_frame frame;
                stack := List.rev_append frames !stack;
                apply cont arg)
              in h args res
            end
        | FHandle fh ->
          if not (EffectLabel.equal l fh.label) then
            grab n (frame :: frames)
          else if n > 0 then
            grab (n-1) (frame :: frames)
          else
            begin match List.nth_opt fh.handlers i with
            | None   -> raise Runtime_error
            | Some h ->
              let res = VFun(fun arg -> VFun(fun res_cont ->
                push_frame (FHandle { fh with cont = res_cont });
                stack := List.rev_append frames !stack;
                apply cont arg))
              in h args res fh.cont
            end
        | FCoerce(c, _) -> grab (Coercion.coerce c l n) (frame :: frames)
        end
      in
    grab n frames
  in

  let rec eval env expr =
    match expr.data with
    | EEffPure -> VEffect Effect.pure
    | EEffId l -> VEffect (Effect.id (Env.lookup_lvar env l))
    | EEffCons(e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VEffect (Effect.cons (to_effect v1) (to_effect v2))
    | ENum n    -> VNum n
    | EChar c   -> VChar c
    | EString s -> VString s
    | EVar x -> Env.lookup_var env x
    | EFun(x, body) ->
      VFun(fun v -> eval (Env.extend env x v) body)
    | EOp(l, n, args, cont) ->
      let l = Env.lookup_lvar env l in
      let args = List.map (eval env) args in
      let cont = eval env cont in
      grab l 0 [] n args cont
    | ECtor(n, args) ->
      VCtor(n, List.map (eval env) args)
    | EApp(e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      apply v1 v2
    | ELet(x, e1, e2) ->
      let v = eval env e1 in
      eval (Env.extend env x v) e2
    | EFix(rfs, e) ->
      let env_ref = ref env in
      let env = List.fold_left (fun env (x, y, body) ->
          Env.extend env x (VFun(fun v ->
            eval (Env.extend !env_ref y v) body))
        ) env rfs in
      env_ref := env;
      eval env e
    | EPureHandle(l, body, return_cl, hs) ->
      let ret v =
        eval (Env.extend env return_cl.pr_arg v) return_cl.pr_body
      in
      let eval_handler h args resume =
        let env = Env.extend_l env h.ph_args args in
        let env = Env.extend env h.ph_res resume in
        eval env h.ph_body
      in
      push_frame (FPureHandle
        { label     = Env.lookup_lvar env l
        ; return_cl = ret
        ; handlers  = List.map eval_handler hs
        });
      eval env body
    | EHandle(l, body, return_cl, hs, cont) ->
      let ret v cont =
        let env = Env.extend env return_cl.r_arg v in
        let env = Env.extend env return_cl.r_cont cont in
        eval env return_cl.r_body
      in
      let eval_handler h args resume cont =
        let env = Env.extend_l env h.h_args args in
        let env = Env.extend env h.h_res resume in
        let env = Env.extend env h.h_cont cont in
        eval env h.h_body
      in
      push_frame (FHandle
        { label     = Env.lookup_lvar env l
        ; return_cl = ret
        ; handlers  = List.map eval_handler hs
        ; cont      = eval env cont
        });
      eval env body
    | EMatch(e, cls) ->
      begin match eval env e with
      | VCtor(n, args) ->
        begin match List.nth_opt cls n with
        | Some(xs, body) ->
          eval (Env.extend_l env xs args) body
        | None -> raise Runtime_error
        end
      | _ -> raise Runtime_error
      end
    | EDone e ->
      begin match pop_frame () with
      | None -> eval env e
      | Some (FPureHandle fh) ->
        fh.return_cl (eval env e)
      | Some (FHandle fh) ->
        fh.return_cl (eval env e) fh.cont
      | Some (FCoerce(_, cont)) ->
        apply cont (eval env e)
      end
    | ENewEffect(l, e) ->
      eval (Env.fresh_effect env l) e
    | EIfPure(e1, e2, e3) ->
      if Effect.is_pure (to_effect (eval env e1)) then
        eval env e2
      else
        eval env e3
    | ECoerce(c, e, cont) ->
      let c = eval_coercion env c in
      push_frame (FCoerce(c, eval env cont));
      eval env e
    | EExtern name ->
      (Predef.DB.get_extern name).Predef.Value.mk_value value_class
    | EReplExpr(e, seal, repl) ->
      let v = eval env e in
      Settings.repl_print_value seal (value_class.Predef.Value.to_string v);
      eval env (Settings.repl seal repl)
    | ERepl(seal, repl) ->
      eval env (Settings.repl seal repl)

  and eval_coercion env c =
    match c.data with
    | CId     -> Coercion.id
    | CLift e -> Coercion.lift (to_effect (eval env e))
    | CSwap(e1, e2) ->
      Coercion.swap (to_effect (eval env e1)) (to_effect (eval env e2))
    | CCons(e, c) ->
      Coercion.cons (to_effect (eval env e)) (eval_coercion env c)
    | CComp(c1, c2) ->
      Coercion.comp (eval_coercion env c1) (eval_coercion env c2)

  in eval Env.empty expr

let flow_node = Flow.Node.create "UCPS value"

let eval_tag = Flow.Tag.create
  ~cmd_flag:  "-eval-ucps"
  ~cmd_descr: " Evaluate a program using UCPS evaluator."
  "UCPS_eval"

let _ =
  Flow.register_transform
    ~contracts: [ eval_tag; CommonTags.eval ]
    ~source: Lang.UCPS.flow_node
    ~target: flow_node
    run
