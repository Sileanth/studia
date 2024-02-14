let inc_z n = ()

let inc_s inc n =
  match n with
  | Z n => S n
  | S n => Z (inc n)
  end

let is_zero_z n = true

let is_zero_s is_zero n =
  match n with
  | Z n => is_zero n
  | _   => false
  end

let inc = inc_s (inc_s (inc_s (inc_s inc_z)))
let is_zero = is_zero_s (is_zero_s (is_zero_s (is_zero_s is_zero_z)))

let rec loop n =
  if is_zero n then n
  else loop (inc n)

in

loop (S (Z (Z (Z ()))))
