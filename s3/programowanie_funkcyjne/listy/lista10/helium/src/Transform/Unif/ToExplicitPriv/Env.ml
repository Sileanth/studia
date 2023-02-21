module VarMap    = Lang.Unif.Var.Map
module TVarMap   = Lang.Unif.TVar.Map
module TConstMap = Lang.Unif.TConst.Map

module T = Lang.Explicit

type t =
  { var_env     : T.var VarMap.t
  ; tvar_env    : Lang.Unif.typ TVarMap.t
  ; tconst_env  : T.TVar.ex TConstMap.t
  ; record_env  : string list TConstMap.t
  ; typedef_env : T.Env.t
  ; type_scope  : Lang.Unif.TConst.Set.t
  }

let init () =
  let open Predef.DB in
  { var_env    = VarMap.empty
  ; tvar_env   = TVarMap.empty
  ; tconst_env =
      List.fold_left (fun m (Type p) ->
        TConstMap.add p.unif_const (T.TVar.Pack p.core_tvar) m
      ) TConstMap.empty (types ())
  ; record_env  = TConstMap.empty
  ; typedef_env = T.Env.empty
  ; type_scope  =
    Lang.Unif.TConst.Set.of_list
      (List.map (fun (Type p) -> p.unif_const) (types ()))
  }

let add_var env x y =
  { env with
    var_env = VarMap.add x y env.var_env
  }

let add_tvar env x tp =
  { env with
    tvar_env = TVarMap.add x tp env.tvar_env
  }

let add_tconst' env c x =
  { env with
    tconst_env  = TConstMap.add c (T.TVar.Pack x) env.tconst_env
  ; typedef_env = T.Env.add_tvar env.typedef_env x
  ; type_scope  = Lang.Unif.TConst.Set.add c env.type_scope
  }

let add_tconst env x =
  let name = Lang.Unif.TConst.full_name x in
  match Kind.tr_kind (Lang.Unif.TConst.kind x) with
  | T.Kind.Pack k ->
    let y = T.TVar.fresh ~name:name k in
    (add_tconst' env x y, T.TVar.Pack y)

let add_tconsts env xs =
  Utils.ListExt.fold_map add_tconst env xs

let add_tconsts' env cs xs =
  List.fold_left2 (fun env c (T.TVar.Pack x) -> add_tconst' env c x) env cs xs

let add_typedef env td =
  { env with
    typedef_env = T.Env.add_typedef env.typedef_env td
  }

let declare_record env l fields =
  { env with
    record_env = TConstMap.add l fields env.record_env
  }

let lookup_var env x =
  VarMap.find x env.var_env

let lookup_tvar env x =
  TVarMap.find_opt x env.tvar_env

let lookup_tconst env x =
  TConstMap.find x env.tconst_env

let lookup_effect env neu =
  T.Env.lookup_effect env.typedef_env neu

let lookup_op env neu n =
  T.Env.lookup_op env.typedef_env neu n

let lookup_adt env neu =
  T.Env.lookup_adt env.typedef_env neu

let lookup_ctor env neu n =
  T.Env.lookup_ctor env.typedef_env neu n

let lookup_record env x =
  TConstMap.find x env.record_env

let is_empty_adt env neu =
  match T.Env.lookup_adt_opt env.typedef_env neu with
  | Some [] -> true
  | _ -> false

let type_scope env = env.type_scope
