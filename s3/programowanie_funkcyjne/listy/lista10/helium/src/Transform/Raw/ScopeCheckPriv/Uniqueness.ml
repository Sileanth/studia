open Lang.Node
module S = Lang.Raw

module StrSet = Set.Make(String)

let check_uniqueness name_of ents =
  let rec loop ns ents =
    match ents with
    | [] -> None
    | ent :: ents ->
      let name = name_of ent in
      if StrSet.mem name ns then Some(ent, name)
      else loop (StrSet.add name ns) ents
  in loop StrSet.empty ents

let check_op_uniqueness ops =
  match check_uniqueness
    (fun { data = S.OpDecl(name, _, _, _) } -> name.data)
    ops
  with
  | None           -> ()
  | Some(op, name) ->
    raise (Errors.operation_redefinition op.meta name)

let check_ctor_uniqueness ctors =
  match check_uniqueness
    (fun { data = S.ADTCtor(name, _, _) } ->
      match name with
      | S.CNName name -> name.data
      | S.CNOper op   -> S.binary_op_var op)
    ctors
  with
  | None             -> ()
  | Some(ctor, name) ->
    raise (Errors.adt_ctor_redefinition ctor.meta name)

let check_field_decl_uniqueness fields =
  match check_uniqueness
    (fun { data = S.FieldDecl(name, _, _) } -> name.data)
    fields
  with
  | None            -> ()
  | Some(fld, name) ->
    raise (Errors.field_redeclaration fld.meta name)

let check_field_pat_uniqueness fpats =
  match check_uniqueness
    (fun { data = S.FieldPat(_, name, _) } -> name.data)
    fpats
  with
  | None             -> ()
  | Some(fpat, name) ->
    raise (Errors.field_redefinition fpat.meta name)

let check_recfun_uniqueness rfs =
  match check_uniqueness
    (fun { data = S.RecFunc(name, _, _, _) } -> name.data)
    rfs
  with
  | None           -> ()
  | Some(rf, name) ->
    raise (Errors.rec_function_redefinition rf.meta name)

let check_field_def_uniqueness fdefs =
  match check_uniqueness
    (fun { data = S.FieldDef(_, name, _) } -> name.data)
    fdefs
  with
  | None -> ()
  | Some(fdef, name) ->
    raise (Errors.field_redefinition fdef.meta name)

let check_typedef_uniqueness tds =
  match check_uniqueness
    (fun td ->
      match td.data with
      | S.TDEffect(name, _, _) | S.TDData(name, _, _)
      | S.TDRecord(name, _, _) ->
        name.data)
    tds
  with
  | None           -> ()
  | Some(td, name) ->
    raise (Errors.type_redefinition td.meta name)
