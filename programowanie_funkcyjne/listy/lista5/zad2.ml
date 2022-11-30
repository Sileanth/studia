
let rec oldfix f x = f (oldfix f) x;;


let mutfix (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) =
  let placeholder (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) : 'b = assert false in
  let fix =  ref placeholder in
  fix := (fun (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) -> f (!fix f) x);
  f (!fix f) x



let lambda_calc_fix f = (fun x -> f (x x)) (fun x -> f (x x)) ;;



type 'a lambda_fix = Fix of ('a lambda_fix -> 'a)
let unfix = function
| Fix f -> f
let fixop f = (fun x a -> f (unfix x x)a ) (Fix (fun x a -> f (unfix x x)a)) ;;
let fix t x = let p (Fix f) = t (f (Fix f)) in 
  p (Fix p) x


let fix f a = (fun (`A x) -> f(x (`A x))) (`A(fun (`A x) y -> f(x (`A x)) y)) a;;












