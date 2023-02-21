open Kind.Datatypes

module Core = struct
  type 'kind t =
    { id   : Utils.UID.t
    ; kind : 'kind kind
    ; name : string
    }
  
  let uid x = x.id

  let gequal x y =
    if Utils.UID.equal x.id y.id then
      Kind.equal x.kind y.kind
    else
      Utils.EqDec.NotEqual
end

module T = struct
  include Core
  module Exists  = Utils.Exists.Make(Core)
  module Map     = Utils.Map1.Make(Core)
  module TVarMap = Map.Make(Core)
end

module Datatypes = struct
  type 'kind tvar = 'kind T.t
end

module type S = sig
  type 'kind t = 'kind T.t

  module Exists : Utils.Exists.S
    with type 'k data = 'k t
    and  type t = T.Exists.t
  include module type of Exists.Datatypes

  module Map : Utils.Map1.S
    with type 'k key = 'k t
    and  type 'v t   = 'v T.Map.t

  module TVarMap : Map.S
    with type 'k v = 'k t
    and  type t = T.TVarMap.t

  val fresh : ?name:string -> 'kind Kind.t -> 'kind t
  val clone : 'kind t -> 'kind t

  val equal : 'k t -> 'k t -> bool

  val kind : 'k t -> 'k Kind.t
  val name : 'k t -> string

  val rename_m : TVarMap.t -> 'k t -> 'k t
end
include T
include T.Exists.Datatypes

let rec default_name_of_kind : type k. k kind -> string =
  function
  | KType   -> "T"
  | KEffect -> "L"
  | KArrow(_, k) -> default_name_of_kind k

let default_name name kind =
  match name with
  | Some name -> name
  | None -> default_name_of_kind kind

let fresh ?name k =
  { id   = Utils.UID.fresh ()
  ; kind = k
  ; name = default_name name k
  }

let clone x =
  { x with id = Utils.UID.fresh () }

let equal x y = Utils.UID.equal x.id y.id

let kind x = x.kind
let name x = x.name

let rename_m tm x =
  match TVarMap.find_opt x tm with
  | Some x -> x
  | None   -> x
