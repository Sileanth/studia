Definition cnat := forall X : Type, (X -> X) -> X -> X.


Definition zero : cnat :=
  fun (X : Type) (f : X -> X) (x : X) => x.


Definition succ (n : cnat) : cnat :=
fun (X : Type) (f : X -> X) (x : X) =>
    f (n X f x).


Definition one : cnat := succ zero.




Definition add (n m : cnat) : cnat :=
    fun (X : Type) (f : X -> X) (x : X) =>
        n X f (m X f x). 
        

Definition mult (n m : cnat) : cnat :=
    fun (X : Type) (f : X -> X) => 
        n X (m X f).
    
Definition mult2 (n m : cnat) : cnat :=
    fun (X : Type) (f : X -> X) =>
        m cnat (add n) zero.

Definition nat_of_cnat (n : cnat) : nat :=
    n nat S O.
    
Fixpoint cnat_of_nat (n : nat) : cnat :=
    match n with
    | O => zero
    | S n => succ (cnat_of_nat n)
    end.

Theorem succ_cnat_nat: forall (n : cnat), S (nat_of_cnat n) = nat_of_cnat ( succ n).
Proof.
    intros n.
    reflexivity.
Qed.


Theorem nat_to_cnat_inection: forall (n : nat), n = nat_of_cnat (cnat_of_nat n).
Proof.
    intros n.
    induction n.
    {
        reflexivity.
    }
    {
        simpl.
        rewrite <- succ_cnat_nat.
        rewrite <-IHn.
        reflexivity.
    }
Qed.


    
Theorem addition_homomorphism: forall (n m : nat), n + m = nat_of_cnat (add (cnat_of_nat n) (cnat_of_nat m)).
Proof.
    intros n.
    induction n.
    {
        intros m.
        simpl.
        assert (H0 : add zero (cnat_of_nat m) = (cnat_of_nat m)).
        {
            reflexivity.
        }
        {
            rewrite H0.
            rewrite <- nat_to_cnat_inection.
            reflexivity.
        }
    }
    {
        intros m.
        simpl.
        assert (H1 : add (succ (cnat_of_nat n)) (cnat_of_nat m) = succ (add (cnat_of_nat n) (cnat_of_nat m))).
        {
            reflexivity.
        }
        {
            rewrite H1.
            rewrite <- succ_cnat_nat.
            rewrite IHn.
            reflexivity.
        }
    }
Qed.

