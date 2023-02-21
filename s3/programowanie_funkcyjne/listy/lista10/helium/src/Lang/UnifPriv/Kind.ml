
type kvar =
  {         id    : Utils.UID.t
  ; mutable value : view option
  }

and view =
| KType
| KEffect
| KVar   of kvar
| KArrow of t * t
and t = view

type kind = t
module KVar = struct
  module Core = struct
    type t = kvar
  end
  include Core

  let equal x y = Utils.UID.equal x.id y.id

  let fresh () =
    { id    = Utils.UID.fresh ()
    ; value = None
    }

  let set x k =
    assert (x.value = None);
    x.value <- Some k

  let set' = set
end

let ktype   = KType
let keffect = KEffect

let arrow k1 k2 = KArrow(k1, k2)

let rec arrows ks rk =
  match ks with
  | [] -> rk
  | k :: ks -> KArrow(k, arrows ks rk)

let rec view k =
  match k with
  | KVar x ->
    begin match x.value with
    | None    -> k
    | Some k' ->
      let r = view k' in
      x.value <- Some r;
      r
    end
  | KType | KEffect | KArrow _ -> k

let fresh_kvar () = KVar(KVar.fresh ())

let rec contains_kvar x k =
  match view k with
  | KVar y -> KVar.equal x y
  | KType | KEffect -> false
  | KArrow(k1, k2) ->
    contains_kvar x k1 || contains_kvar x k2

let contains_kvar' = contains_kvar
