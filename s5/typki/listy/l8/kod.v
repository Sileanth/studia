Require Import Coq.Arith.Gt.
Require Import Coq.Arith.Wf_nat.


Lemma xd : forall a b c (H1 : a <= b) (H2 : S b = c), c <> a.
Proof.
  intros a b c h1 h2.
  subst.
  inversion h1.
  +  subst. auto.
  +  subst. inversion H. 
    - subst. Nat.order.