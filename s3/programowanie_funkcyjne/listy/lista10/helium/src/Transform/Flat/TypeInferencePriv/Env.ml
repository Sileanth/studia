module S = Lang.Flat
module T = Lang.Unif

type op_decl =
  { op_effect      : Lang.Unif.tconst
  ; op_effect_args : Lang.Unif.tconst list
  ; op_index       : int
  ; op_decl        : Lang.Unif.op_decl
  }

type adt_ctor =
  { ctor_datatype      : Lang.Unif.tconst
  ; ctor_datatype_args : Lang.Unif.tconst list
  ; ctor_index         : int
  ; ctor_decl          : Lang.Unif.adt_ctor
  }

type field_decl =
  { fld_record      : Lang.Unif.tconst
  ; fld_record_args : Lang.Unif.tconst list
  ; fld_index       : int
  ; fld_decl        : Lang.Unif.field_decl
  }

type tconst =
| TConst of Lang.Unif.tconst
| TAlias of Lang.Unif.typ

type bool_type =
  { bt_type      : Lang.Unif.typ
  ; bt_true_pat  : Lang.Unif.pattern
  ; bt_false_pat : Lang.Unif.pattern
  }

type unit_type =
  { ut_type  : Lang.Unif.typ
  ; ut_value : Lang.Unif.expr
  }

type list_type =
  { lt_nil_name   : string
  ; lt_nil_proxy  : Lang.Unif.var
  ; lt_nil_ctor   : adt_ctor
  ; lt_cons_name  : string
  ; lt_cons_proxy : Lang.Unif.var
  ; lt_cons_ctor  : adt_ctor
  }

type time_stamp =
| TS_At     of int
| TS_Before of int

type special =
  { bool_type       : bool_type option
  ; unit_type       : unit_type option
  ; list_type       : list_type option
  ; printer_type    : Lang.Unif.tconst option
  ; printer_interp  : Lang.Flat.expr option
  ; default_printer : Lang.Unif.var option
  ; printers        : Lang.Unif.var list
  }

type t =
  { time         : int
  ; var_map      : T.var S.Var.Map.t
  ; op_map       : op_decl S.Op.Map.t
  ; ctor_map     : adt_ctor S.Ctor.Map.t
  ; field_map    : field_decl S.Field.Map.t
  ; op_proxies   : T.var S.Op.Map.t
  ; ctor_proxies : T.var S.Ctor.Map.t
  ; field_sels   : T.var S.Field.Map.t
  ; tconst_map   : tconst S.TConst.Map.t
  ; time_stamps  : time_stamp T.TConst.Map.t
  ; typedefs     : SyntaxExt.typedef T.TConst.Map.t
  ; type_scope   : T.TConst.Set.t
  ; special      : special
  }

let empty =
  { time         = 0
  ; var_map      = S.Var.Map.empty
  ; op_map       = S.Op.Map.empty
  ; ctor_map     = S.Ctor.Map.empty
  ; field_map    = S.Field.Map.empty
  ; op_proxies   = S.Op.Map.empty
  ; ctor_proxies = S.Ctor.Map.empty
  ; field_sels   = S.Field.Map.empty
  ; tconst_map   = S.TConst.Map.empty
  ; typedefs     = T.TConst.Map.empty
  ; time_stamps  = T.TConst.Map.empty
  ; type_scope   = T.TConst.Set.empty
  ; special      =
    { bool_type       = None
    ; unit_type       = None
    ; list_type       = None
    ; printer_type    = None
    ; printer_interp  = None
    ; default_printer = None
    ; printers        = []
    }
  }

let add_var env x sch =
  let y = T.Var.fresh ~name:(S.Var.name x) sch in
  { env with
    var_map = S.Var.Map.add x y env.var_map
  }, y

let add_op env l op =
  { env with
    op_map = S.Op.Map.add l op env.op_map
  }

let add_ctor env l ctor =
  { env with
    ctor_map = S.Ctor.Map.add l ctor env.ctor_map
  }

let add_field env l fld =
  { env with
    field_map = S.Field.Map.add l fld env.field_map
  }

let add_op_proxy env x sch =
  let y = T.Var.fresh ~name:(S.Op.name x) sch in
  { env with
    op_proxies = S.Op.Map.add x y env.op_proxies
  }, y

let add_ctor_proxy env x sch =
  let y = T.Var.fresh ~name:(S.Ctor.name x) sch in
  { env with
    ctor_proxies = S.Ctor.Map.add x y env.ctor_proxies
  }, y

let add_field_sel env x sch =
  let y = T.Var.fresh ~name:(S.Field.name x) sch in
  { env with
    field_sels = S.Field.Map.add x y env.field_sels
  }, y

let extend_tconst env ?(fresh=false) c =
  let ts = if fresh then TS_At env.time else TS_Before env.time in
  { env with
    time        = if fresh then env.time + 1 else env.time
  ; time_stamps = T.TConst.Map.add c ts env.time_stamps
  ; type_scope  = T.TConst.Set.add c env.type_scope
  }

let add_tconst0 env ~fresh x c =
  let env =
    { env with
      tconst_map = S.TConst.Map.add x (TConst c) env.tconst_map
    } in
  (extend_tconst env ~fresh:fresh c, c)

let add_tconst env ?(fresh=false) ?kind x =
  let name  = S.TConst.name x in
  let mpath = S.TConst.mpath x in
  let kind = match kind with None -> T.Kind.fresh_kvar () | Some k -> k in
  let c    = T.TConst.fresh ~name (mpath, kind) in
  add_tconst0 env ~fresh:fresh x c

let add_tconst' env ?(fresh=false) name kind =
  let c = T.TConst.fresh ~name ([], kind) in
  (extend_tconst env ~fresh:fresh c, c)

let add_type_alias env x tp =
  { env with
    tconst_map = S.TConst.Map.add x (TAlias tp) env.tconst_map
  }

let add_typedef env l td =
  { env with
    typedefs = T.TConst.Map.add l td env.typedefs
  }

let init () =
  let open Predef.DB in
  let env = empty in
  let env = List.fold_left (fun env (Type p) ->
      fst (add_tconst0 env ~fresh:false p.flat_const p.unif_const)
    ) env (types ())
  in env

let lookup_var env x =
  S.Var.Map.find x env.var_map

let lookup_op env l =
  S.Op.Map.find l env.op_map

let lookup_ctor env l =
  S.Ctor.Map.find l env.ctor_map

let lookup_field env l =
  S.Field.Map.find l env.field_map

let lookup_tconst env x =
  S.TConst.Map.find x env.tconst_map

let lookup_op_proxy env l =
  S.Op.Map.find l env.op_proxies

let lookup_ctor_proxy env l =
  S.Ctor.Map.find l env.ctor_proxies

let lookup_field_sel env l =
  S.Field.Map.find l env.field_sels

let lookup_typedef env l =
  T.TConst.Map.find_opt l env.typedefs

(* ========================================================================= *)

let register_bool_type env bt =
  { env with
    special = { env.special with bool_type = Some bt }
  }

let register_unit_type env ut =
  { env with
    special = { env.special with unit_type = Some ut }
  }

let register_list_type env lt =
  { env with
    special = { env.special with list_type = Some lt }
  }

let register_printer_type env pt =
  { env with
    special = { env.special with printer_type = Some pt }
  }

let register_printer_interp env pi =
  { env with
    special = { env.special with printer_interp = Some pi }
  }

let register_default_printer env p =
  { env with
    special = { env.special with default_printer = Some p }
  }

let register_printer env p =
  { env with
    special = { env.special with printers = p :: env.special.printers }
  }

let bool_type ~pos env =
  match env.special.bool_type with
  | Some bt -> bt
  | None ->
    failwith "Bool type not registered"

let unit_type ~pos env =
  match env.special.unit_type with
  | Some ut -> ut
  | None ->
    failwith "Unit type not registered"

let list_type ~pos env =
  match env.special.list_type with
  | Some lt -> lt
  | None ->
    failwith "List type not registered"

let printer_type    env = env.special.printer_type
let printer_interp  env = env.special.printer_interp
let default_printer env = env.special.default_printer
let printers        env = env.special.printers

(* ========================================================================= *)

let can_swap env c1 c2 =
  let time_stamp_before ts1 ts2 =
    match ts1, ts2 with
    | TS_At t1, TS_At t2     -> t1 < t2
    | TS_Before t1, TS_At t2 -> t1 <= t2
    | _, TS_Before _         -> false
  in
  let ts1 = T.TConst.Map.find c1 env.time_stamps in
  let ts2 = T.TConst.Map.find c2 env.time_stamps in
  time_stamp_before ts1 ts2 || time_stamp_before ts2 ts1

let tvars env =
  let fv = T.TVar.Set.empty in
  let fv = S.Var.Map.fold (fun _ x fv ->
      T.TVar.Set.union fv (T.Scheme.tvars (T.Var.scheme x))
    ) env.var_map fv in
  let fv = S.Op.Map.fold (fun _ x fv ->
      T.TVar.Set.union fv (T.Scheme.tvars (T.Var.scheme x))
    ) env.op_proxies fv in
  let fv = S.Ctor.Map.fold (fun _ x fv ->
      T.TVar.Set.union fv (T.Scheme.tvars (T.Var.scheme x))
    ) env.ctor_proxies fv in
  let fv = S.TConst.Map.fold (fun _ tc fv ->
      match tc with
      | TConst _  -> fv
      | TAlias tp -> T.TVar.Set.union fv (T.Type.tvars tp)
    ) env.tconst_map fv in
  let fv = T.TConst.Map.fold (fun _ td fv ->
      T.TVar.Set.union fv (T.TypeDef.tvars (SyntaxExt.get_typedef td))
    ) env.typedefs fv in
  fv

let type_scope env = env.type_scope
