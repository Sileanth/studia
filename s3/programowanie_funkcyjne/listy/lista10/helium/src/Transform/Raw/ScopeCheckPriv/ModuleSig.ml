open Lang.Node

type decl = (Utils.Position.t, decl_data) node
and decl_data =
| DeclType of Lang.Flat.tconst * Lang.Flat.kind
| DeclVal  of Lang.Flat.field * Lang.Flat.var *
    Lang.Flat.type_farg list * Lang.Flat.type_expr

type impl_kind =
| ImplRecord
| ImplEmpty of Lang.Flat.ctor

type struct_s =
  { ss_repr_type : Lang.Flat.tconst
  ; ss_repr_ctor : Lang.Flat.ctor
  ; ss_impl_type : Lang.Flat.tconst
  ; ss_impl_kind : impl_kind
  ; ss_decls     : decl list
  }

type functor_s =
  { fs_repr_type  : Lang.Flat.tconst
  ; fs_repr_field : Lang.Flat.field
  ; fs_arg_type   : module_type
  ; fs_val_type   : module_type
  }

and module_type =
| TStruct  of struct_s
| TFunctor of functor_s

let repr_type msig =
  match msig with
  | TStruct  str  -> str.ss_repr_type
  | TFunctor fctr -> fctr.fs_repr_type

let impl_type msig =
  match msig with
  | TStruct  str  -> str.ss_impl_type
  | TFunctor fctr -> fctr.fs_repr_type
