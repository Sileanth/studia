open Lang.CoreCommon.Kind.Datatypes

type predef_type =
| Type :
  { kind        : 'k kind
  ; flat_const  : Lang.Flat.tconst
  ; unif_const  : Lang.Unif.tconst
  ; core_tvar   : 'k Lang.Core.tvar
  ; untyped_var : Lang.Untyped.var option
  } -> predef_type

let type_list  = ref []
let core_env   = ref Lang.Core.Env.empty

let register_type tp =
  let Type p = tp in
  type_list := tp :: !type_list;
  core_env  := Lang.Core.Env.add_tvar !core_env p.core_tvar

let types  () = List.rev !type_list

let core_env () = !core_env

let extern_map = Hashtbl.create 32
let register_extern name v =
  Hashtbl.add extern_map name v

let extern_exists name = Hashtbl.mem extern_map name

let get_extern name = Hashtbl.find extern_map name
