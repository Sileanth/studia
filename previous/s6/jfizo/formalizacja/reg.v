


Inductive Vector (A : Type) : (n : nat) -> Type :=
  | nil : Vector A 0
  | cons : A -> Vector A n -> Vector A (n + 1).