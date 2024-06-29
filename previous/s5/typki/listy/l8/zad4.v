Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Classes.RelationClasses.
Require Import Coq.Lists.List.
Require Import Coq.Arith.Gt.
Require Import Coq.Arith.Wf_nat.

Class Kripke {W : Type} (R : relation W) (idx : W -> nat) := {
  refl_kripke : Reflexive R ;
  trans_kripke : Transitive R ;
  idx_mono : forall w1 w2, R w1 w2 -> (w1 <> w2) -> (idx w1) > (idx w2) ;
}.

Inductive formula (V : Type) : Type :=
  | f_var : V -> formula V
  | f_and : formula V -> formula V -> formula V
  | f_or  : formula V -> formula V -> formula V
  | f_top : formula V
  | f_bot : formula V
  | f_imp : formula V -> formula V -> formula V
  | f_lat : formula V -> formula V
.
Arguments f_var {V}.
Arguments f_and {V}.
Arguments f_or  {V}.
Arguments f_top {V}.
Arguments f_bot {V}.
Arguments f_imp {V}.
Arguments f_lat {V}.

Definition f_not {V : Type} (ff : formula V) : formula V := f_imp ff f_bot.
Arguments f_not {V}.

Class Monotone {W : Type} (R : relation W) {V : Type} (f : V -> W -> bool) := {
  monotone : forall w1 w2 v, R w1 w2 -> (f v w1 = true) -> (f v w2 = true);
}.

Section Zadanie4.
Variable (W : Type).
Variable (R : relation W).
Variable (idx : W -> nat).
Variable (KM : Kripke R idx).

Fixpoint semant {V} (f : V -> W -> bool)
  `{MF : Monotone W R V f} (w : W) (e : formula V) : Prop := 
  match e with
  | f_top     => True
  | f_bot     => False
  | f_var v   => f v w = true
  | f_and l r => semant f w l /\ semant f w r 
  | f_or  l r => semant f w l \/ semant f w r 
  | f_imp l r => forall (w' : W), R w w' -> (semant f w' l -> semant f w' r)
  | f_lat ee  => forall (w' : W), R w w' -> (w <> w') -> (semant f w' ee)
  end.

Definition models {V : Type} (asm: list (formula V)) (e : formula V) :=
  forall (f : V -> W -> bool) (m : Monotone R f) (w : W) ,
  Forall (@semant V f m w) asm -> @semant V f m w e.

Require Import Coq.Arith.Lt.
Lemma strong_ind (P : nat -> Prop) :
  (forall m, (forall k, k < m -> P k) -> P m) -> forall n, P n.
Proof.
  intros H n; enough (H0: forall p, p <= n -> P p).
  - apply H0, le_n.
  - induction n.
    + intros. inversion H0. apply H. intros. inversion H2.
    + intros. apply H. intros. apply IHn. inversion H0.
      * rewrite H2 in H1. apply Lt.lt_n_Sm_le in H1. assumption.
      * specialize (PeanoNat.Nat.lt_le_trans k p n H1 H3). apply PeanoNat.Nat.lt_le_incl.
Qed.

(*
Fixpoint str_ind {V : Type}
  (f : V -> W -> bool)
  `{MF : Monotone W R V f}
  (phi : formula V)
  (w w' : W)
  (iw' : nat)
  (HR : R w w')
  (Hne : w <> w')
  (H' : semant f w (f_lat phi) -> semant f w phi) : semant f w (f_lat phi)*)

Lemma neeeq (w1 w2 w3 : W) (H : R w1 w2) (H' : R w2 w3) (Hne : w1 <> w2) (Hne' : w2 <> w3)
  : w1 <> w3.
Proof.
  remember (idx w1) as iw1.
  remember (idx w2) as iw2.
  remember (idx w3) as iw3.
      assert (Hgt : iw2 > iw3). {
        rewrite Heqiw2, Heqiw3.
        eapply idx_mono; eassumption.
      }
      assert (Hgt2 : iw1 > iw2). {
        rewrite Heqiw1, Heqiw2.
         eapply idx_mono; eassumption.
      }
      assert (Hgg : iw1 > iw3). {
        apply (gt_trans iw1 iw2 iw3); assumption.
      }
      rewrite Heqiw1 in Hgg.
      rewrite Heqiw3 in Hgg.
      intros c. rewrite c in Hgg. rewrite <- Heqiw3 in Hgg. unfold gt in Hgg.
      apply Nat.lt_irrefl in Hgg. assumption.
Qed.

Lemma semant_mono {V} (f : V -> W -> bool) `{MF : Monotone W R V f}
  (w : W) (w' : W) (HR : R w w') (Hneq : w <> w') (phi : formula V) (H' : semant f w phi)
  : semant f w' phi.
Proof.
  induction phi.
  - cbn. cbn in H'. eapply monotone; eassumption.
  - cbn. cbn in H'. destruct H'. split.
    + apply IHphi1. assumption.
    + apply IHphi2. assumption.
  - cbn. cbn in H'. destruct H'.
    + left. apply IHphi1. assumption.
    + right. apply IHphi2. assumption.
  - easy.
  - inversion H'.
  - cbn in H'. cbn. intros w0 HR0 H2.
    apply H'.
    + eapply trans_kripke; eassumption.
    + apply H2.
  - cbn. intros w0 HR0 Hne.
    cbn in H'. apply H'.
    + eapply trans_kripke; eassumption.
    + eapply neeeq; eassumption.
Qed.

(*
Theorem induction_ltof1 :
  forall P : A -> Type,
    (forall x:A, (forall y:A, ltof y x -> P y) -> P x) -> forall a:A, P a.

*)

Theorem zadanie4a : forall {V : Type} (gamma: list (formula V)) (phi : formula V),
  models (cons (f_lat phi) gamma) phi -> models gamma phi.
Proof.
  intros V gamma phi H.
  unfold models. unfold models in H.
  intros f m w.
  intros Hgamma.
  apply H.
  rewrite Forall_cons_iff; split; [ | assumption ].
  specialize (H f m).
  cbn.
  intros w1 HR Hne.
  Print induction_ltof1.
  induction w1 using (induction_ltof1 _ idx).
  unfold ltof in H0.
  apply H.
  rewrite Forall_cons_iff; split.
  - cbn.
    intros w2 HR2 Hne2.
    apply H0.
    + apply idx_mono; assumption.
    + eapply trans_kripke; eassumption.
    + eapply neeeq; eassumption.
  - clear phi H H0.
    induction gamma.
    + auto.
    + rewrite Forall_cons_iff in Hgamma. destruct Hgamma.
      rewrite Forall_cons_iff; split.
      * eapply semant_mono; eassumption.
      * apply IHgamma, H0.
Qed.




Theorem zadanie4b : forall {V : Type} (sigma gamma : list (formula V)) (phi : formula V),
  models (sigma ++ gamma) phi -> models ((map f_lat sigma) ++ gamma) (f_lat phi).
Proof.
  intros V sigma gamma phi H.
  unfold models. unfold models in H.
  intros f m w Hassm.
  specialize (H f m).
  cbn. intros w' HR Hne.
  apply H.
  rewrite Forall_app in Hassm; destruct Hassm.
  rewrite Forall_app; split.
  - clear gamma phi H H1.
    induction sigma.
    + auto.
    + rewrite map_cons in H0. rewrite Forall_cons_iff in H0. destruct H0.
      rewrite Forall_cons_iff; split.
      * cbn in H. apply H; assumption.
      * apply IHsigma, H0.
  - clear sigma phi H H0.
    induction gamma.
    + auto.
    + rewrite Forall_cons_iff in H1. destruct H1.
      rewrite Forall_cons_iff; split.
      * eapply semant_mono; eassumption.
      * apply IHgamma, H0.
Qed.


