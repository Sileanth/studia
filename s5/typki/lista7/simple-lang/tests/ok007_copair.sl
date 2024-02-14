let idx v =
  case v of inl x => 0 | inr x => 1
in

(idx (inl true), idx (inr false))
