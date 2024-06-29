
(*
Fixpoint ack (m n : nat) : nat := 
  match m, n with 
  | O, _       => S n
  | S m', O     => ack m' 1
  | S m', S n' => ack m' (ack m n')
  end.
Cannot guess decreasing argument of fix.
*)

Fixpoint iter (f : nat -> nat) (z : nat) (n : nat) : nat :=
  match n with 
  | O => z
  | S n' => iter f (f z) n'
   end.

Fixpoint ack (n m : nat) : nat := 
  match n with 
  | O   => S m
  | S n' => iter (ack n') (ack n' 1) m
  end.

Theorem ack_case1 : forall (m : nat), ack O m = S m.
Proof. simpl. reflexivity. Qed.

Theorem ack_case2 : forall (n: nat), ack (S n) O = ack n 1.
Proof. simpl. reflexivity. Qed.

Theorem ack_case3 : forall (n m: nat), ack (S n) (S m) = ack n (ack (S n) m).
Proof. 
  intros n. induction n.
  + intros m.
    repeat (rewrite <- ack_case1).
  


