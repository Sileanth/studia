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

| red_ep_vp : ∀ (v1 v2 : value A),
    red (e_pair v1 v2) (v_pair v1 v2)

| red_ep1 : ∀ e₁ e₁' e₂,
    red e₁ e₁' →
    red (e_pair e₁ e₂) (e_pair e₁' e₂)

| red_ep2 : ∀ (v : value A) e e',
    red e e' →
    red (e_pair v e) (e_pair v e')

| red_efst : ∀ e e',
    red e e' →
    red (e_fst e) (e_fst e')

| red_esnd : ∀ e e',
    red e e' →
    red (e_snd e) (e_snd e')

| red_vfst : ∀ (v1 v2 : value A),
    red (e_fst (v_pair v1 v2)) v1

| red_vsnd : ∀ (v1 v2 : value A),
    red (e_snd (v_pair v1 v2)) v2
.