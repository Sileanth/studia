
let rec oldfix f x = f (oldfix f) x;;


let mutfix (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) =
  let placeholder (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) : 'b = assert false in
  let fix =  ref placeholder in
  fix := (fun (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) -> f (!fix f) x);
  f (!fix f) x




type 'a lambda_fix = Fix of ('a lambda_fix -> 'a)


  let unfix = function
  | Fix f -> f

  let y t = let p (Fix f) x = t (f (Fix f)) x in 
    p (Fix p)

let fix f = (fun x a -> f (unfix x x) a) (Fix (fun x a -> f (unfix x x) a));;

let notfix f = (fun x -> f (x x)) (fun x -> f (x x));;
let fix f = (fun (`A x) -> f(x (`A x))) (`A(fun (`A x) y -> f(x (`A x)) y));;


