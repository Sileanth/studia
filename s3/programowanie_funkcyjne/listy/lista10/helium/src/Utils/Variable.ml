
module type VarDescr = sig
  type typ

  val default_name : string
end

module type S = sig
  type typ

  type t

  val clone : ?name:string -> t -> t
  val fresh : ?name:string -> ?meta:Seal.t -> typ -> t
  
  val name    : t -> string
  val meta    : t -> Seal.t
  val type_of : t -> typ
  val uid     : t -> UID.t

  val equal : t -> t -> bool

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module Make(D : VarDescr) : S with type typ := D.typ = struct
  module Core = struct
    type t =
      { meta : Seal.t
      ; typ  : D.typ
      ; name : string
      ; id   : UID.t
      }

    let compare x y = UID.compare x.id y.id
  end
  include Core

  let clone ?name x =
    { x with
      id = UID.fresh ()
    ; name = (match name with None -> x.name | Some name -> name)
    }

  let fresh ?(name=D.default_name) ?(meta=Seal.empty) typ =
    { meta = meta
    ; typ  = typ
    ; name = name
    ; id   = UID.fresh ()
    }

  let name    x = x.name
  let meta    x = x.meta
  let type_of x = x.typ
  let uid     x = x.id

  let equal x y = UID.equal x.id y.id

  module Map = Map.Make(Core)
  module Set = Set.Make(Core)
end
