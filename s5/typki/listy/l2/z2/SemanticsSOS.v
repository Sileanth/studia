(** This module provides the SOS semantics *)

Require Import Utf8.
Require Import Syntax.

(** SOS reduction rules *)
Inductive red {A : Set} : expr A → expr A → Prop :=
| red_beta : ∀ e (v : value A),
    red (e_app (v_lam e) v) (esubst e v)

| red_app1 : ∀ e₁ e₁' e₂,
    red e₁ e₁' →
    red (e_app e₁ e₂) (e_app e₁' e₂)

| red_app2 : ∀ (v : value A) e e',
    red e e' →
    red (e_app v e) (e_app v e')

| red_case : ∀ e e' e1 e2,
    red e e' ->
    red (e_case e e1 e2) (e_case e' e1 e2)

| red_lcase : ∀ v e1 e2,
    red (e_case (v_inl v) e1 e2) (esubst e1 v)

| red_rcase : ∀ v e1 e2,
    red (e_case (v_inr v) e1 e2) (esubst e2 v)

| red_einl : ∀ e e',
    red e e' →
    red (e_inl e) (e_inl e')

| red_einr : ∀ e e',
    red e e' →
    red (e_inr e) (e_inr e')

| red_vinl : ∀ (v : value A),
    red (e_inl v) (v_inl v)

| red_vinr : ∀ (v : value A),
    red (e_inr v) (v_inr v)
.