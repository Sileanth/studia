open Type.Datatypes

type 'eff select_result =
| Selected of 'eff
| Opened   of 'eff
| NotPossible

(** select_at_effect env l eff selects atomic effect l from row eff.
 returns:
 * Selected eff' when eff is equivalent to l :: eff'
 * Opened   eff' when eff can be opened with l
     and effect l can be swapped from the end of eff
     (eff' is equal to eff)
 * NotPossible   when effect l cannot be selected from eff
*)
val select_at_effect :
  Env.t -> at_effect -> at_effect list -> at_effect list select_result

(** select_effect env eff1 eff2 selects effect eff1 from eff2.
  returns:
  * Selected eff' when eff2 is equivalent to eff1 :: eff'
  * Opened   eff' when eff2 is a subtype of eff1 :: eff'
      and subtyping derivation uses open rules
  * NotPossible   when effect eff1 cannot be selected from eff2
*)
val select_effect : Env.t -> effect -> effect -> effect select_result

val equiv : Env.t -> 'k typ -> 'k typ -> bool

(** match_type t1 t2 checks if t1 <: t2 *)
val match_type : Env.t -> 'k typ -> 'k typ -> bool

val glb_type : Env.t -> 'k typ -> 'k typ -> 'k typ
val lub_type : Env.t -> 'k typ -> 'k typ -> 'k typ

val lub_types : Env.t -> 'k typ list -> 'k typ
