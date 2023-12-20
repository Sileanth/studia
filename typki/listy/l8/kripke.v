Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Classes.RelationClasses.




  

Class Kripke_Model {W : Type} (R : relation W) `{ReflKripke : Reflexive W R} `{TransKripke : Transitive W R} := {

}.




Inductive formula {V : Type} : Type :=
  | Var : V -> formula 
  | And : formula -> formula -> formula 
  | Or  : formula -> formula -> formula 
  | Top : formula
  | Bot : formula
  | Imp : formula -> formula -> formula.

Arguments Var {V}.
Arguments And {V}.
Arguments Or {V}.
Arguments Top {V}.
Arguments Bot {V}.
Arguments Imp {V}.


(* R a b =def a >= b*)

Class MonotoneFunction (W: Type) (R : relation W) (V: Type) (f : W -> V -> bool)   `{km : Kripke_Model W R} := {
  monotone : forall (a b : W) (v : V), R a b -> (f a v = true) -> (f b v = true);
}.


Fixpoint semant {W V : Type} (R : relation W) (f : W -> V -> bool) `{MF : MonotoneFunction W R V f} (w : W)  (ff : formula) : Prop := 
match ff with
| Top     => True
| Bot     => False
| Var v   => f w v = true
| And l r => semant R f w l /\ semant R f w r 
| Or  l r => semant R f w l \/ semant R f w r 
| Imp l r => forall (w' : W), R w w' -> (semant R f w' l -> semant R f w' r)
end.

Inductive IntWorld :=
| Source : IntWorld
| End : IntWorld.

Inductive IntRel : IntWorld -> IntWorld -> Prop :=
| SS : IntRel Source Source
| EE : IntRel End End
| SE : IntRel Source End.


#[export] Instance int_rel_refl : Reflexive IntRel.
Proof.
  unfold Reflexive. intros x. destruct x; constructor.
Qed.

#[export] Instance int_rel_trans : Transitive IntRel.
Proof.
  unfold Transitive. intros x y z rel_xy rel_yz. inversion rel_xy; subst; inversion rel_yz; subst; constructor.
Qed.

#[export] Instance int_rel_kripke : Kripke_Model IntRel := {
}.

Inductive IntVar :=
| P : IntVar.

Definition wart (w : IntWorld) (v : IntVar) : bool :=
match w with
| Source => false
| End => true
end.

Search (S _ <= S _).
  
#[export] Instance wart_monotone_function : MonotoneFunction IntWorld IntRel IntVar wart.
Proof.
  constructor.
  intros a b v rel_ab a_true.
  inversion rel_ab; subst. simpl; try constructor. simpl in a_true. inversion a_true.
  constructor.
  constructor.
Qed.

Theorem lem_dont_work : semant IntRel wart Source (Or (Var P) (Imp (Var P) (Bot))) -> False.
Proof.
  simpl. intros. destruct H.
  - inversion H.
  - specialize (H End SE). simpl in H. apply H. constructor.
Qed.