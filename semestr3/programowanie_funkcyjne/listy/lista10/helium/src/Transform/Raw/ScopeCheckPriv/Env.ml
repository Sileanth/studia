open Lang.Node
module T = Lang.Flat

type var = Namespace.var =
| VVar  of T.var
| VOp   of T.op
| VCtor of T.ctor

type module_v = Namespace.module_v =
| MNamespace of Namespace.t
| MFunctor   of Lang.Flat.var * ModuleSig.functor_s

type t =
  { req_modules  : Context.def list list ref
  ; opened_ns    : Namespace.t
  ; current_ns   : Namespace.t
  ; current_path : string list
  ; file_finder  : Namespace.t FileFinder.t
  ; tvar_map     : (string, (Utils.Position.t * T.tconst)) Hashtbl.t option
  }

let add_item env ns_add x y =
  { env with
    opened_ns  = ns_add env.opened_ns  x y
  ; current_ns = ns_add env.current_ns x y
  }

let add_var env x =
  let y = T.Var.fresh ~name:x () in
  (add_item env Namespace.add_var x y, y)

let add_tconst env ~local x =
  let y = T.TConst.fresh ~name:x (if local then [] else env.current_path) in
  (add_item env Namespace.add_tconst x y, y)

let set_typedef env x ns =
  add_item env Namespace.set_typedef x ns

let add_op' env op =
  add_item env Namespace.add_op (T.Op.name op) op

let add_ctor' env ctor =
  add_item env Namespace.add_ctor (T.Ctor.name ctor) ctor

let add_field' env fld =
  add_item env Namespace.add_field (T.Field.name fld) fld

let add_module env name mv =
  add_item env Namespace.add_module name mv

let add_namespace env name ns =
  add_module env name (MNamespace ns)

(* ========================================================================= *)

let file_action req_modules init_ns tr_defs file_finder ~mpath ?intf fname =
  let env =
    { req_modules  = req_modules
    ; opened_ns    = init_ns
    ; current_ns   = Namespace.empty
    ; current_path = mpath
    ; file_finder  = file_finder
    ; tvar_map     = None
    }
  in
  let (ns, defs) = tr_defs env ?intf fname in
  req_modules := defs :: !req_modules;
  ns

let init tr_defs =
  let open Predef.DB in
  let ns = Namespace.empty in
  let ns = List.fold_left (fun ns (Type p) ->
      Namespace.add_tconst ns (T.TConst.name p.flat_const) p.flat_const
    ) ns (types ()) in
  let req_modules = ref [] in
  let finder =
    FileFinder.init
      ~lib_dirs:     (Settings.lib_dirs ())
      (file_action req_modules ns tr_defs)
  in
  let ns =
    match Settings.prelude_module () with
    | None       -> ns
    | Some pname ->
      begin match FileFinder.find finder pname with
      | Some pns -> Namespace.merge pns ns
      | None     -> raise (Errors.unbound_module_np pname)
      end
  in
  let finder = FileFinder.update finder (file_action req_modules ns tr_defs) in
  List.iter (fun name -> FileFinder.find finder name |> ignore)
    (Settings.required_libs ());
  { req_modules  = req_modules
  ; opened_ns    = ns
  ; current_ns   = Namespace.empty
  ; current_path = []
  ; file_finder  = FileFinder.update finder
      ~current_path: (Settings.current_file_path ())
      ~source_dirs:  (Settings.source_dirs ())
      (file_action req_modules ns tr_defs)
  ; tvar_map     = None
  }

(* ========================================================================= *)

let new_namespace env =
  { env with
    current_ns = Namespace.empty
  }

let extend_path env name =
  { env with
    current_path = env.current_path @ [ name ]
  }

let set_path env path =
  { env with
    current_path = path
  }

(* ========================================================================= *)

let allow_tvars env =
  { env with
    tvar_map = Some (Hashtbl.create 32)
  }

let lookup_tvar env x =
  match env.tvar_map with
  | None     -> raise (Errors.tvar_not_allowed x.meta)
  | Some map ->
    begin match Hashtbl.find_opt map x.data with
    | Some(_, x) -> x
    | None       ->
      let y = T.TConst.fresh ~name:x.data [] in
      Hashtbl.add map x.data (x.meta, y);
      y
    end

let get_tvars env =
  match env.tvar_map with
  | None     -> []
  | Some map ->
    Hashtbl.fold (fun _ (pos, x) xs ->
      { meta = pos; data = T.TFA_Var x } :: xs
    ) map []

(* ========================================================================= *)

let rec follow_mpath mname ns mpath =
  match mpath with
  | [] -> (mname, ns)
  | m :: mpath ->
    begin match Namespace.lookup_module ns m.data with
    | Some(MNamespace ns) ->
      follow_mpath (mname ^ "." ^ m.data) ns mpath
    | Some(MFunctor _) ->
      raise (Errors.module_kind_mismatch m.meta
        Errors.MKFunctor Errors.MKStruct)
    | None    -> raise
      (Errors.not_module_member m.meta Errors.N_Module m.data mname)
    end

let resolve_mpath env m mpath =
  match Namespace.lookup_module env.opened_ns m.data with
  | Some(MNamespace ns) ->
    follow_mpath m.data ns mpath
  | Some(MFunctor _) ->
    raise (Errors.module_kind_mismatch m.meta
      Errors.MKFunctor Errors.MKStruct)
  | None    ->
    begin match FileFinder.find env.file_finder m.data with
    | Some ns -> follow_mpath m.data ns mpath
    | None    -> raise (Errors.unbound m.meta Errors.N_Module m.data)
    end

let simple_lookup no ns_lookup env mpath x =
  match mpath with
  | [] ->
    begin match ns_lookup env.opened_ns x.data with
    | Some x -> x
    | None   -> raise (Errors.unbound x.meta no x.data)
    end
  | m :: mpath ->
    let (mname, ns) = resolve_mpath env m mpath in
    begin match ns_lookup ns x.data with
    | Some x -> x
    | None   -> raise (Errors.not_module_member x.meta no x.data mname)
    end

let module_like_lookup no ns_lookup of_namespace env mpath x =
  match mpath with
  | [] ->
    begin match ns_lookup env.opened_ns x.data with
    | Some x -> x
    | None   ->
      begin match FileFinder.find env.file_finder x.data with
      | Some ns ->
        begin match of_namespace ns with
        | Some x -> x
        | None   -> raise (Errors.unbound x.meta no x.data)
        end
      | None -> raise (Errors.unbound x.meta no x.data)
      end
    end
  | m :: mpath ->
    let (mname, ns) = resolve_mpath env m mpath in
    begin match ns_lookup ns x.data with
    | Some x -> x
    | None   -> raise (Errors.not_module_member x.meta no x.data mname)
    end

(* ========================================================================= *)

let lookup_var   = simple_lookup Errors.N_Var   Namespace.lookup_var
let lookup_op    = simple_lookup Errors.N_Op    Namespace.lookup_op
let lookup_ctor  = simple_lookup Errors.N_Ctor  Namespace.lookup_ctor
let lookup_field = simple_lookup Errors.N_Field Namespace.lookup_field

let lookup_tconst_base =
  module_like_lookup Errors.N_Type
    Namespace.lookup_tconst_base
    (fun ns -> Namespace.lookup_tconst_base ns "this")

let lookup_tconst env mpath x =
  match lookup_tconst_base env mpath x with
  | Namespace.TCConst x       -> x
  | Namespace.TCTypedef(x, _) -> x

let lookup_typedef env mpath x =
  match lookup_tconst_base env mpath x with
  | Namespace.TCTypedef(_, ns) -> ns
  | Namespace.TCConst _ ->
    raise (Errors.type_elems_not_visible x.meta x.data)

let lookup_infix_ctor env x =
  let name = Lang.Raw.binary_op_var x in
  match Namespace.lookup_var env.opened_ns name with
  | Some (VCtor c) -> c
  | Some _ -> raise (Errors.not_infix_ctor x.meta name)
  | None   -> raise (Errors.unbound x.meta Errors.N_Ctor name)

let lookup_module =
  module_like_lookup Errors.N_Module
    Namespace.lookup_module
    (fun ns -> Some (MNamespace ns))

let lookup_namespace env mpath x =
  match lookup_module env mpath x with
  | MNamespace ns -> ns
  | MFunctor _ ->
    raise (Errors.module_kind_mismatch x.meta
      Errors.MKFunctor Errors.MKStruct)

(* ========================================================================= *)

let current_namespace env = env.current_ns
let current_path env = env.current_path

let open_namespace env ns =
  { env with
    opened_ns = Namespace.merge ns env.opened_ns
  }

let include_namespace env ns =
  { env with
    opened_ns  = Namespace.merge ns env.opened_ns
  ; current_ns = Namespace.merge ns env.current_ns
  }

let auto_open env mname =
  match FileFinder.find env.file_finder mname with
  | Some ns -> open_namespace env ns
  | None    -> raise (Errors.unbound_module_np mname)

let req_modules_rev env =
  !(env.req_modules)

let clean_req_modules env =
  env.req_modules := []
