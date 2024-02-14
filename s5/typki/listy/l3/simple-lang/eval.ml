(** The evaluator *)

type value =
  | VUnit
  | VBool of bool
  | VNum  of int
  | VFn   of (value -> value)
  | VPair of value * value
  | VInl  of value
  | VInr  of value
  | VRecord  of (string * value) list
  | VVariant of string * value

module Env : sig
  type t

  val empty : t

  val extend : t -> SystemF.var -> value -> t

  val lookup : t -> SystemF.var -> value
end = struct
  module StrMap = Map.Make(String)

  type t = value StrMap.t

  let empty = StrMap.empty

  let extend env x v = StrMap.add x v env

  let lookup env x =
    match StrMap.find_opt x env with
    | None   -> failwith ("runtime error: unbound variable " ^ x)
    | Some v -> v
end

let rec eval env (e : SystemF.expr) =
  match e with
  | EUnit   -> VUnit
  | EBool b -> VBool b
  | ENum  n -> VNum  n
  | EVar  x -> Env.lookup env x
  | EFn(x, _, body) ->
    VFn (fun v -> eval (Env.extend env x v) body)
  | EFix(f, x, _, _, e) ->
    let rec func v =
      eval (Env.extend (Env.extend env f (VFn func)) x v) e
    in
    VFn func
  | EApp(e1, e2) ->
    begin match eval env e1 with
    | VFn f -> f (eval env e2)
    | _ -> failwith "runtime error"
    end
  | ETFn(_, e)  -> eval env e
  | ETApp(e, _) -> eval env e
  | ERFn(_, e)  -> eval env e
  | ERApp(e, _) -> eval env e
  | ELet(x, e1, e2) ->
    eval (Env.extend env x (eval env e1)) e2
  | EPair(e1, e2) ->
    VPair (eval env e1, eval env e2)
  | EFst e ->
    begin match eval env e with
    | VPair(v, _) -> v
    | _ -> failwith "runtime error"
    end
  | ESnd e ->
    begin match eval env e with
    | VPair(_, v) -> v
    | _ -> failwith "runtime error"
    end
  | EInl(e, _) -> VInl (eval env e)
  | EInr(_, e) -> VInr (eval env e)
  | ECase(e, (x1, e1), (x2, e2)) ->
    begin match eval env e with
    | VInl v -> eval (Env.extend env x1 v) e1
    | VInr v -> eval (Env.extend env x2 v) e2
    | _ -> failwith "runtime error"
    end
  | EIf(e1, e2, e3) ->
    begin match eval env e1 with
    | VBool true  -> eval env e2
    | VBool false -> eval env e3
    | _ -> failwith "runtime error"
    end
  | ESeq(e1, e2) ->
    begin match eval env e1 with
    | VUnit -> eval env e2
    | _ -> failwith "runtime error"
    end
  | EAbsurd(e, _) | EMatchEmpty(e, _) ->
    let _ = eval env e in
    failwith "runtime error"
  | ESelect(e, l) ->
    begin match eval env e with
    | VRecord r ->
      begin match List.assoc_opt l r with
      | Some v -> v
      | None -> failwith "runtime error"
      end
    | _ -> failwith "runtime error"
    end
  | ERecord flds ->
    VRecord (List.map (fun (l, e) -> (l, eval env e)) flds)
  | ECtor(l, e, _) ->
    VVariant(l, eval env e)
  | EMatch(e, l, (x1, e1), (x2, e2)) ->
    begin match eval env e with
    | VVariant(l', v) when l = l' ->
      eval (Env.extend env x1 v) e1
    | VVariant _ as v->
      eval (Env.extend env x2 v) e2
    | _ -> failwith "runtime error"
    end
    
let rec pp_value v =
  match v with
  | VUnit   -> "()"
  | VBool b -> string_of_bool b
  | VNum  n -> string_of_int  n
  | VFn   _ -> "<fn>"
  | VPair(v1, v2) -> Printf.sprintf "(%s, %s)" (pp_value v1) (pp_value v2)
  | VInl v -> Printf.sprintf "inl(%s)" (pp_value v)
  | VInr v -> Printf.sprintf "inr(%s)" (pp_value v)
  | VRecord [] -> "{}"
  | VRecord((l, v) :: r) ->
    List.fold_left
      (fun s (l, v) -> Printf.sprintf "%s, %s=%s" s l (pp_value v))
      (Printf.sprintf "{%s=%s" l (pp_value v))
      r ^ "}"
  | VVariant(l, v) ->
    Printf.sprintf "%s(%s)" l (pp_value v)

let eval_program (p : SystemF.program) =
  eval Env.empty p |> pp_value |> print_endline
