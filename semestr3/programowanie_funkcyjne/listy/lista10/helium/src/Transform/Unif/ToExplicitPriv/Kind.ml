module S = Lang.Unif
module T = Lang.Explicit

let rec tr_kind k =
  match S.Kind.view k with
  | S.KVar _  -> T.Kind.Pack T.KType
  | S.KType   -> T.Kind.Pack T.KType
  | S.KEffect -> T.Kind.Pack T.KEffect
  | S.KArrow(k1, k2) ->
    let T.Kind.Pack k1 = tr_kind k1 in
    let T.Kind.Pack k2 = tr_kind k2 in
    T.Kind.Pack(T.KArrow(k1, k2))
