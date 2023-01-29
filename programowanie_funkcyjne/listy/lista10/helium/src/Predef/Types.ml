open Lang.CoreCommon.Kind.Datatypes

module type PredefinedType = sig
  type t_kind

  val unif_tp     : Lang.Unif.typ
  val unif_scheme : Lang.Unif.scheme
  val core_tp     : t_kind Lang.Core.typ

  val info : DB.predef_type
end

module type TypeDecscr = sig
  type t_kind
  val kind : t_kind kind
  val name : string
end

module MakeType(T : TypeDecscr) :
    PredefinedType with type t_kind := T.t_kind =
struct
  let rec unif_kind : type k. k kind -> Lang.Unif.kind = function
    | KType   -> Lang.Unif.Kind.ktype
    | KEffect -> Lang.Unif.Kind.keffect
    | KArrow(k1, k2) -> Lang.Unif.Kind.arrow (unif_kind k1) (unif_kind k2)

  let rec effect_like : type k. k kind -> bool = function
    | KType   -> false
    | KEffect -> true
    | KArrow(_, k) -> effect_like k

  let flat_const =
    Lang.Flat.TConst.fresh ~name: T.name []

  let core_tvar =
    Lang.Core.TVar.fresh ~name: T.name T.kind

  let unif_const =
    Lang.Unif.TConst.fresh ~name: T.name ([], unif_kind T.kind)

  let untyped_var =
    if effect_like T.kind then
      Some (Lang.Untyped.Var.fresh ~name: T.name ())
    else
      None

  let unif_tp     = Lang.Unif.Type.tconst unif_const
  let unif_scheme = Lang.Unif.Scheme.close_with [] unif_tp
  let core_tp     = Lang.Core.Type.var core_tvar

  let info = DB.Type
    { kind        = T.kind
    ; flat_const  = flat_const
    ; unif_const  = unif_const
    ; core_tvar   = core_tvar
    ; untyped_var = untyped_var
    }

  let _ =
    DB.register_type info
end

module Int = MakeType(struct
  type t_kind = k_type
  let kind = KType
  let name = "Int"
end)

module Char = MakeType(struct
  type t_kind = k_type
  let kind = KType
  let name = "Char"
end)

module String = MakeType(struct
  type t_kind = k_type
  let kind = KType
  let name = "String"
end)

module IO = MakeType(struct
  type t_kind = k_effect
  let kind = KEffect
  let name = "IO"
end)
