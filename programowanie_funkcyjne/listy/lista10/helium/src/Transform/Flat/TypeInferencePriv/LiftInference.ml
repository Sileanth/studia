module U = Lang.Unif

let to_tconst tp =
  match U.Type.nf_view tp with
  | U.TNF_Neu(U.HConst c, _) -> Some c
  | U.TNF_Neu(U.HVar _, _) -> None
  | U.TNF_EffPure | U.TNF_EffCons _ | U.TNF_Arrow _ | U.TNF_Fun _ -> None

let can_explicitly_swap env l1 l2 =
  match !Settings.auto_lift with
  | Settings.ALP_No     -> false
  | Settings.ALP_Normal ->
    begin match to_tconst l1, to_tconst l2 with
    | Some c1, Some c2 -> not (U.TConst.equal c1 c2)
    | _,       _       -> false
    end

let lift_effect_l l eff =
  match !Settings.auto_lift with
  | Settings.ALP_No     -> None
  | Settings.ALP_Normal -> Some (U.ECLift (l, eff))
