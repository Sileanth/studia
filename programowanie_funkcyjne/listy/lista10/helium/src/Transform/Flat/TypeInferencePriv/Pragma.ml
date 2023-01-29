open Lang.Node
module S = Lang.Flat
module E = SyntaxExt
module T = Lang.Unif

let pragma_flag ~pos env flag cont =
  cont env

(* ========================================================================= *)

let pragma_val ~pos env flag x cont =
  match flag with
  | "InterpretPrinter" ->
    cont (Env.register_printer_interp env
      { meta = pos; data = Lang.Flat.EVar x })
  | "DefaultPrinter" ->
    let x = Env.lookup_var env x in
    cont (Env.register_default_printer env x)
  | "Printer" ->
    let x = Env.lookup_var env x in
    cont (Env.register_printer env x)
  | _ -> cont env

(* ========================================================================= *)

let as_typedef env x =
  match Env.lookup_tconst env x with
  | Env.TConst x -> Env.lookup_typedef env x
  | Env.TAlias _ -> None

let pragma_bool_type ~pos env x =
  let make data = { meta = pos; data = data } in
  match as_typedef env x with
  | Some(E.TDData(x, [], [ false_ctor; true_ctor ])) ->
    let ctor_pattern n (T.ADTCtor(_, ets, tps)) =
      let args = List.map (fun tp ->
          make (T.PWildcard (T.Scheme.close_with [] tp))
        ) tps in
      make (T.PCtor(x, [], n, ets, args))
    in
    Env.register_bool_type env
    { Env.bt_type      = T.Type.tconst x
    ; Env.bt_true_pat  = ctor_pattern 1 (snd true_ctor).data
    ; Env.bt_false_pat = ctor_pattern 0 (snd false_ctor).data
    }
  | Some(E.TDData(_, [], _)) ->
    failwith "Bool type must have exactly two constructors"
  | Some(E.TDData(_, _ :: _, _)) ->
    failwith "Bool type must have no type parameters"
  | _ ->
    failwith "Bool type must be an ADT with two constructors"

let pragma_unit_type ~pos env x =
  match as_typedef env x with
  | Some(E.TDData(x, [], (_, { data = T.ADTCtor(_, [], []) }) :: _)) ->
    let tp = T.Type.tconst x in
    Env.register_unit_type env
    { Env.ut_type  = tp
    ; Env.ut_value =
      { meta =
        { T.em_pos    = pos
        ; T.em_type   = tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.ECtor(x, [], 0, [])
      }
    }
  | Some(E.TDData(_, [], _ :: _)) ->
    failwith "First constructor of unit type must have no parameters"
  | Some(E.TDData(_, _ :: _, _)) ->
    failwith "Unit type must have no type parameters"
  | _ ->
    failwith "Unit type must be a non-empty ADT"

let pragma_list_type ~pos env x =
  match as_typedef env x with
  | Some(E.TDData(x, targs, [ (nil, nil_decl); (cons, cons_decl) ])) ->
    Env.register_list_type env
    { Env.lt_nil_name   = S.Ctor.name nil
    ; Env.lt_nil_proxy  = Env.lookup_ctor_proxy env nil
    ; Env.lt_nil_ctor   =
      { Env.ctor_datatype      = x
      ; Env.ctor_datatype_args = targs
      ; Env.ctor_index         = 0
      ; Env.ctor_decl          = nil_decl
      }
    ; Env.lt_cons_name  = S.Ctor.name cons
    ; Env.lt_cons_proxy = Env.lookup_ctor_proxy env cons
    ; Env.lt_cons_ctor  =
      { Env.ctor_datatype      = x
      ; Env.ctor_datatype_args = targs
      ; Env.ctor_index         = 1
      ; Env.ctor_decl          = cons_decl
      }
    }
  | _ ->
    failwith "List type must be an ADT with two constructors"

let pragma_printer_type ~pos env x =
  match Env.lookup_tconst env x with
  | Env.TConst x ->
    begin try
      Unification.unify_kind (T.TConst.kind x)
        (T.Kind.arrow T.Kind.ktype T.Kind.ktype)
    with
    | Unification.Cannot_unify ->
      failwith "Printer type must have kind (type -> type)."
    end;
    Env.register_printer_type env x
  | _ ->
    failwith "Type alias cannot be registered as Printer type"

let pragma_type ~pos env flag x cont =
  match flag with
  | "BoolType"    -> cont (pragma_bool_type ~pos env x)
  | "UnitType"    -> cont (pragma_unit_type ~pos env x)
  | "ListType"    -> cont (pragma_list_type ~pos env x)
  | "PrinterType" -> cont (pragma_printer_type ~pos env x)
  | _ -> cont env
