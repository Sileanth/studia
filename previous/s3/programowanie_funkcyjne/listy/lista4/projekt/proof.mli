open Logic

type proof

(** Tworzy pusty dowód podanego twierdzenia *)
val proof : (string * formula) list -> formula -> proof

(** Zamienia ukończony dowód na twierdzenie *)
val qed : proof -> theorem

val aph : formula -> proof -> proof
(** Jeśli dowód jest ukończony, zwraca None. W przeciwnym wypadku
  zwraca Some(Γ, φ), gdzie Γ oraz φ to odpowiednio dostępne
  założenia oraz formuła do udowodnienia w aktywnym podcelu *)
val goal : proof -> ((string * formula) list * formula) option

(** Przesuwa cyklicznie aktywny podcel na następny (od lewej do prawej) *)
val next : proof -> proof

(** Wywołanie intro name pf odpowiada regule wprowadzania implikacji.
  To znaczy aktywna dziura wypełniana jest regułą:

  (nowy aktywny cel)
   (name,ψ) :: Γ ⊢ φ
   -----------------(→I)
       Γ ⊢ ψ → φ

  Jeśli aktywny cel nie jest implikacją, wywołanie kończy się błędem *)
val intro : string -> proof -> proof

(** Wywołanie apply ψ₀ pf odpowiada jednocześnie eliminacji implikacji
  i eliminacji fałszu. Tzn. jeśli do udowodnienia jest φ, a ψ₀ jest
  postaci ψ₁ → ... → ψₙ → φ to aktywna dziura wypełniana jest regułami
  
  (nowy aktywny cel) (nowy cel)
        Γ ⊢ ψ₀          Γ ⊢ ψ₁
        ----------------------(→E)  (nowy cel)
                ...                   Γ ⊢ ψₙ
                ----------------------------(→E)
                            Γ ⊢ φ

  Natomiast jeśli ψ₀ jest postaci ψ₁ → ... → ψₙ → ⊥ to aktywna dziura
  wypełniana jest regułami

  (nowy aktywny cel) (nowy cel)
        Γ ⊢ ψ₀          Γ ⊢ ψ₁
        ----------------------(→E)  (nowy cel)
                ...                   Γ ⊢ ψₙ
                ----------------------------(→E)
                            Γ ⊢ ⊥
                            -----(⊥E)
                            Γ ⊢ φ *)
val apply : formula -> proof -> proof

(** Wywołanie apply_thm thm pf
 działa podobnie do apply (Logic.consequence thm) pf, tyle że
 aktywna dziura od razu jest wypełniana dowodem thm.
 Nowa aktywna dziura jest pierwszą na prawo po tej, która została
 wypełniona przez thm *)

val apply_thm : theorem -> proof -> proof

(** Wywołanie apply_assm name pf
  działa tak jak apply (Logic.by_assumption f) pf,
  gdzie f jest założeniem o nazwie name *)
val apply_assm : string -> proof -> proof

val pp_print_proof : Format.formatter -> proof -> unit

