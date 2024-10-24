(** Here, we prove progress and type preservation.
  * For substitution lemma refer to Typing module. *)

Require Import Utf8.
Require Import Syntax SemanticsSOS Typing.

(** Inversion lemma about closed values of an arrow type *)
Lemma arrow_value_inv {Γ : env Empty_set} {v : value _} {τ₁ τ₂ : type} :
  T[ Γ ⊢ v ∷ t_arrow τ₁ τ₂ ] →
  ∃ e, v = v_lam e ∧ T[ env_ext Γ τ₁ ⊢ e ∷ τ₂ ].
Proof.
  inversion 1.
  + destruct x.
  + eauto.
Qed.

Lemma union_value_inv {Γ : env Empty_set} {v : value _} {τ₁ τ₂ : type} :
  T[ Γ ⊢ v ∷ t_union τ₁ τ₂ ] →
  ∃ v', v_inl v' = v ∨ v_inr v' = v.
Proof.
  inversion 1.
  + destruct x.
  + eauto.
  + eauto.
Qed.

(*Lemma union_value_inv2 {Γ : env Empty_set} {v : value _} {τ₁ τ₂ : type} :
  T[ Γ ⊢ v ∷ t_union τ₁ τ₂ ] →
  (∃ v', v_inl v' = v ) ∨ (∃ v', v_inr v' = v).
Proof.
  inversion 1.
  + destruct x.
  + eauto.
  + eauto.
Qed.*)

Theorem progress (e : expr _) (τ : type) :
  T[ env_empty ⊢ e ∷ τ ] →
  (∃ v : value _, e = v) ∨ (∃ e', red e e').
Proof.
  (* Since the type of potentially free variables is an irregular parameter
   * (may change on recursive call) of the typing relation, the induction
   * scheme generated by Coq requires to prove the propery for all these sets.
   * Therefore we use Henry Ford induction here: we prove the property for any
   * set A so long as it is Empty_set. We start with permuting the assumption
   * to setup scene for the induction *)
  generalize env_empty as Γ; remember Empty_set as A.
  intros Γ Htyping; generalize HeqA; clear HeqA.
  (* And now, we can proceed by induction on the typing derivation *)
  induction Htyping as [ | | | A Γ e₁ e₂ τ₂ τ₁ Htyping1 IH1 ? IH2 | | | | |];
    intro Heq; subst.
  + left; eexists; reflexivity.
  + destruct x. (* impossible case *)
  + left; eexists; reflexivity.
  + right.
    (* we have three cases: *)
    destruct IH1 as [ [ v₁ ? ] | [ e₁' ? ] ];
      [ reflexivity
      | destruct IH2 as [ [ v₂ ? ] | [ e₂' ? ] ]; trivial | ].
    - subst.
      destruct (arrow_value_inv Htyping1) as [ e [ Hv He ] ].
      subst; eexists; apply red_beta.
    - subst; eexists; apply red_app2; eassumption.
    - eexists; apply red_app1; eassumption.
  + destruct IHHtyping; auto.
    - right. destruct H. rewrite H. econstructor. apply red_vinl.
    - right. destruct H. eexists. apply red_einl. eauto.
  + destruct IHHtyping; auto.
    - right. destruct H. rewrite H. econstructor. apply red_vinr.
    - right. destruct H. eexists. apply red_einr. eauto.
  + left. econstructor. eauto.
  + left. econstructor. eauto.
  + right. destruct IHHtyping1; auto.
    - destruct H. rewrite H in Htyping1. rewrite H.
      destruct (union_value_inv Htyping1).
      destruct H0.
      * rewrite <- H0. eexists. apply red_lcase.
      * rewrite <- H0. eexists. apply red_rcase.
    - destruct H. eexists. apply red_case. eauto. 
Qed.

(* ========================================================================= *)

Theorem preservation {A : Set} (Γ : env A) e e' τ :
  red e e' →
  T[ Γ ⊢ e ∷ τ ] → T[ Γ ⊢ e' ∷ τ ].
Proof.
  (* We need slightly generalize the theorem, to say: foeach τ ... *)
  intro Hred; generalize τ; clear τ.
  (* And proceed by induction on the derivation of reduction judgement *)
  induction Hred; intro τ.
  + intro H.
    repeat match goal with
    | [ H: T[ _ ⊢ e_app _ _ ∷ _ ] |- _ ] => inversion H; clear H; subst
    | [ H: T[ _ ⊢ e_value (v_lam _) ∷ _ ] |- _ ] => inversion H; clear H; subst
    end.
    eapply typing_subst; eassumption.
  + inversion 1; subst; econstructor; eauto.
  + inversion 1; subst; econstructor; eauto.
  + inversion 1; subst; econstructor; eauto.
  + inversion 1; subst. eapply typing_subst.
    - inversion H3. eauto.
    - easy.
  + inversion 1; subst. eapply typing_subst.
    - inversion H3. eauto.
    - easy.
  + intro. inversion H. apply T_Inl. apply IHHred. apply H1.
  + intro. inversion H. apply T_Inr. apply IHHred. apply H1.
  + intro. inversion H. apply T_VInl. apply H1.
  + intro. inversion H. apply T_VInr. apply H1.
Qed.















