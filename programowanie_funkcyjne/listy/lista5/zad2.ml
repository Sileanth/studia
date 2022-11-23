
let rec oldfix f x = f (oldfix f) x;;





let mutfix (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) =
  let placeholder (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) : 'b = failwith "abc" in
  let fix =  ref placeholder in
  fix := (fun (f : (('a -> 'b) -> 'a -> 'b)) (x : 'a) -> f (!fix f) x);
  f (!fix f) x
 

type 'a fixik =
| Fix of 'a * (unit -> 'a fixik)
