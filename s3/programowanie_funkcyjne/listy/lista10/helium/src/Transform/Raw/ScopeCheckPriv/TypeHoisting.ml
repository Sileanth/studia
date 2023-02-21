open Lang.Node
module T = Lang.Flat

module TSet = T.TConst.Set

type t =
  { new_targs : T.type_farg list
  ; hoisted   : TSet.t
  }

let build_app ~pos tp args =
  let make data = { meta = pos; data = data } in
  List.fold_left (fun tp arg ->
    match arg.data with
    | T.TFA_Var x | T.TFA_Annot(x, _) ->
      make (T.TApp(tp, make (T.TConst x)))
  ) tp args

let rec tr_type h tp =
  { tp with data =
    match tp.data with
    | T.TPlaceholder -> T.TPlaceholder
    | T.TEffPure     -> T.TEffPure
    | T.TConst x     ->
      if TSet.mem x h.hoisted then
        (build_app ~pos:tp.meta tp h.new_targs).data
      else
        T.TConst x
    | T.TArrowPure(tp1, tp2) ->
      T.TArrowPure(tr_type h tp1, tr_type h tp2)
    | T.TArrowEff(tp1, tp2, eff) ->
      T.TArrowEff(tr_type h tp1, tr_type h tp2, tr_type h eff)
    | T.TEffRow eff ->
      T.TEffRow (tr_type h eff)
    | T.TEffCons(l, eff) ->
      T.TEffCons(tr_type h l, tr_type h eff)
    | T.TApp(tp1, tp2) ->
      T.TApp(tr_type h tp1, tr_type h tp2)
  }

let tr_op_decl h op =
  { op with data =
    match op.data with
    | T.OpDecl(op, ets, tps, tp) ->
      T.OpDecl(op, ets, List.map (tr_type h) tps, tr_type h tp)
  }

let tr_adt_ctor h ctor =
  { ctor with data =
    match ctor.data with
    | T.ADTCtor(ctor, ets, tps) ->
      T.ADTCtor(ctor, ets, List.map (tr_type h) tps)
  }

let tr_field_decl h fld =
  { fld with data =
    match fld.data with
    | T.FieldDecl(fld, ets, tp) ->
      T.FieldDecl(fld, ets, tr_type h tp)
  }

let tr_typedef h td =
  { td with data =
    match td.data with
    | T.TDEffect(x, fargs, ops) ->
      T.TDEffect(x,
        (if TSet.mem x h.hoisted then h.new_targs @ fargs else fargs),
        List.map (tr_op_decl h) ops)
    | T.TDData(x, fargs, ctors) ->
      T.TDData(x,
        (if TSet.mem x h.hoisted then h.new_targs @ fargs else fargs),
        List.map (tr_adt_ctor h) ctors)
    | T.TDRecord(x, fargs, flds) ->
      T.TDRecord(x,
        (if TSet.mem x h.hoisted then h.new_targs @ fargs else fargs),
        List.map (tr_field_decl h) flds)
  }

let hoist_typedefs fargs tds =
  let h =
    { new_targs = fargs
    ; hoisted   = List.fold_left (fun hs td ->
        match td.data with
        | T.TDEffect(x, _, _) | T.TDData(x, _, _) | T.TDRecord(x, _, _) ->
          TSet.add x hs
      ) TSet.empty tds
    } in
  (h, List.map (tr_typedef h) tds)
