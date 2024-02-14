let swap p = (snd p, fst p)

let foo x =
  case x of
  | inl v => inr (v, x)
  | inr z => inl (swap z)

in

foo (foo (inl 42))
