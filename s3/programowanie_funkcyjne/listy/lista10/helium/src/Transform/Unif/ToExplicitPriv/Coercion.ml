open Lang.Node

module S = Lang.Unif
module T = Lang.Explicit

let pack_coercion data =
  { meta = Utils.Seal.empty
  ; data = data}

let rec tr_eff_coercion env c =
  pack_coercion
  (match c with
   | S.ECId eff        ->
     T.ECId (Type.tr_effect env eff)
   | S.ECOpen l        ->
     T.ECLift (Type.tr_effect env l)
   | S.ECLift (l, eff) ->
     let l   = Type.tr_effect env l
     and eff = Type.tr_effect env eff in
     T.ECConsR(pack_coercion (T.ECLift l), eff)
   | S.ECSwap(l1, l2, eff) ->
     let l1  = Type.tr_effect env l1
     and l2  = Type.tr_effect env l2
     and eff = Type.tr_effect env eff in
     T.ECConsR (pack_coercion (T.ECSwap (l1, l2)), eff)
   | S.ECCons(l, c)   ->
     T.ECConsL(Type.tr_effect env l, tr_eff_coercion env c)
   | S.ECComp(c1, c2) ->
     T.ECComp(tr_eff_coercion env c1, tr_eff_coercion env c2))

let rec find_arrow tp =
  match S.Type.nf_view tp with
  | S.TNF_Arrow (ta, tr, er) -> ta, tr, er
  | S.TNF_EffCons _ | S.TNF_EffPure | S.TNF_Neu _ | S.TNF_Fun _ ->
     failwith "Internal type error (not an arrow) during translation (Unif -> Explicit)"

let rec tr_val_coercion env c =
  { meta = Utils.Seal.empty
  ; data =
    match c with
    | S.VCId -> T.VCId
    | S.VCArrow(c1, c2, c3) ->
      T.VCArrow(tr_val_coercion env c1,
        tr_val_coercion env c2,
        tr_eff_coercion env c3)
  }
