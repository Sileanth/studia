
(* 9 *)
val xd = [2, 3, 4, 5, 6]
fun suffixes xs =
	case xs of
	  [] => [[]]
	| y :: ys => xs :: (suffixes ys)


(* 10 *)

fun foldl f c (x :: xs) = foldl f (f x c) xs
  | foldl f c [] = c


fun foldr f c (x :: xs) = f x (foldr f c xs)
  | foldr f c [] = c


fun foldr2 f c (x :: xs) con = foldr2 f c xs (fn y => con (f x y))
  | foldr2 f c [] con = con c

fun foldrI f c xs = foldr2 f c xs (fn x => x)
