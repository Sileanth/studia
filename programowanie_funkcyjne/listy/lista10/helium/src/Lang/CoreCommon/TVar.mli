
module T : sig
  type 'kind t

  module Exists  : Utils.Exists.S with type 'k data = 'k t
  module Map     : Utils.Map1.S   with type 'k key  = 'k t
  module TVarMap : Map.S with type 'k v = 'k t
end

module Datatypes : sig
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
include S
