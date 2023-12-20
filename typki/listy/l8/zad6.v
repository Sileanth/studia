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

Section Zadanie6.
Definition W := nat.
Definition R := ge.
Definition idx : nat -> nat := fun n => n.
#[export] Instance km_nat : Kripke R idx.
Proof.
  split.
  - unfold R. auto.
  - unfold R. intros x y z H1 H2. eapply Nat.le_trans; eassumption.
  - intros w1 w2 HR Hne.
    unfold idx. unfold R in HR. unfold ge in HR. unfold gt. unfold lt. Nat.order.
Qed.

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

Lemma neq_trans (w1 w2 w3 : W) (H : R w1 w2) (H' : R w2 w3) (Hne : w1 <> w2) (Hne' : w2 <> w3)
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
    + eapply neq_trans; eassumption.
Qed.

Definition models {V : Type} (e : formula V) :=
  forall (f : V -> W -> bool) (m : Monotone R f) (w : W), @semant V f m w e.

  Lemma xd : forall a b c (H1 : a <= b) (H2 : S b = c), c <> a.
  Proof.
    intros a b c H1 H2. subst.
    intros w. subst.
    apply Nat.nle_succ_diag_l in H1.
    assumption.
  Qed.


Theorem zadanie6 : forall {V : Type} (p q : formula V)
  (f : V -> W -> bool) (m : Monotone R f) (w : W),
  @semant V f m w (f_lat (f_imp p q)) <-> @semant V f m w (f_imp (f_lat p) (f_lat q)).
Proof.
  intros V p q f m w.
  split.
  - intros H.
    cbn in H. unfold ge in H.
    remember (f_lat p) as pp.
    remember (f_lat q) as qq.
    cbn. subst.
    intros w1 HRww1 H1.
    cbn. intros w2 HRw1w2 Hne12.
    cbn in H1. specialize (H1 w2 HRw1w2 Hne12).
    specialize (H w2).
    apply H.
    + eapply trans_kripke; eassumption.
    + unfold R, ge in H. unfold R, ge in HRww1. unfold R, ge in HRw1w2. Nat.order.
    + apply refl_kripke.
    + assumption.
  - intros H.
    cbn in H. cbn.
    intros w1 HRww1 Hneww1.
    intros w2 HRw1w2 H1.
    assert (Haha : R w (S w2)). {
     unfold R, ge. unfold R, ge in HRww1. unfold R, ge in HRw1w2. Nat.order.
    }
    specialize (H (S w2) Haha).
    apply H.
    + intros w' HR Hne. unfold R in HR.
      assert (Haha2 : R w2 w'). {
        unfold R, ge.
        unfold ge in HR.
        inversion HR; Nat.order.
      }
      inversion Haha2.
      * assumption.
      * eapply semant_mono.
        -- apply Haha2.
        -- eapply xd; eassumption.
        -- assumption.
    + unfold R. auto.
    + auto.
Qed.
