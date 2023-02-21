module T = Lang.Flat

module StrMap = Map.Make(String)

type var =
| VVar  of T.var
| VOp   of T.op
| VCtor of T.ctor

type tconst =
| TCConst   of Lang.Flat.tconst
| TCTypedef of Lang.Flat.tconst * t

and module_v =
| MNamespace of t
| MFunctor   of Lang.Flat.var * ModuleSig.functor_s

and t =
  { var_map    : var StrMap.t
  ; op_map     : T.op StrMap.t
  ; ctor_map   : T.ctor StrMap.t
  ; field_map  : T.field StrMap.t
  ; tconst_map : tconst StrMap.t
  ; module_map : module_v StrMap.t
  }

let empty =
  { var_map    = StrMap.empty
  ; op_map     = StrMap.empty
  ; ctor_map   = StrMap.empty
  ; field_map  = StrMap.empty
  ; tconst_map = StrMap.empty
  ; module_map = StrMap.empty
  }

let merge ns1 ns2 =
  let merge_str_map m1 m2 = StrMap.union (fun k v1 v2 -> Some v1) m1 m2 in
  { var_map    = merge_str_map ns1.var_map    ns2.var_map
  ; op_map     = merge_str_map ns1.op_map     ns2.op_map
  ; ctor_map   = merge_str_map ns1.ctor_map   ns2.ctor_map
  ; field_map  = merge_str_map ns1.field_map  ns2.field_map
  ; tconst_map = merge_str_map ns1.tconst_map ns2.tconst_map
  ; module_map = merge_str_map ns1.module_map ns2.module_map
  }

(* ========================================================================= *)

let add_var ns x y =
  { ns with
    var_map = StrMap.add x (VVar y) ns.var_map
  }

let add_op ns name op =
  { ns with
    var_map = StrMap.add name (VOp op) ns.var_map
  ; op_map  = StrMap.add name op ns.op_map
  }

let add_ctor ns name ctor =
  { ns with
    var_map  = StrMap.add name (VCtor ctor) ns.var_map
  ; ctor_map = StrMap.add name ctor ns.ctor_map
  }

let add_field ns name fld =
  { ns with
    field_map = StrMap.add name fld ns.field_map
  }

let add_tconst ns x y =
  { ns with
    tconst_map = StrMap.add x (TCConst y) ns.tconst_map
  }

let add_module ns name m =
  { ns with
    module_map = StrMap.add name m ns.module_map
  }

let set_typedef ns x td =
  let name = T.TConst.name x in
  assert (StrMap.mem name ns.tconst_map);
  { ns with
    tconst_map = StrMap.add name (TCTypedef(x, td)) ns.tconst_map
  }

let add_typedef ns name x td =
  { ns with
    tconst_map = StrMap.add name (TCTypedef(x, td)) ns.tconst_map
  }

(* ========================================================================= *)

let lookup_var ns x =
  StrMap.find_opt x ns.var_map

let lookup_op ns x =
  StrMap.find_opt x ns.op_map

let lookup_ctor ns x =
  StrMap.find_opt x ns.ctor_map

let lookup_field ns x =
  StrMap.find_opt x ns.field_map

let lookup_module ns x =
  StrMap.find_opt x ns.module_map

let rec lookup_functor ns x =
  match lookup_module ns x with
  | Some(MNamespace ns) -> lookup_functor ns "this"
  | Some(MFunctor(x, fs)) -> Some(x, fs)
  | None -> None

let rec lookup_tconst_base ns x =
  match StrMap.find_opt x ns.tconst_map with
  | Some x -> Some x
  | None ->
    begin match lookup_module ns x with
    | Some(MNamespace ns) -> lookup_tconst_base ns "this"
    | Some(MFunctor _) -> None
    | None -> None
    end

let lookup_tconst ns x =
  match lookup_tconst_base ns x with
  | Some(TCConst x)       -> Some x
  | Some(TCTypedef(x, _)) -> Some x
  | None -> None
