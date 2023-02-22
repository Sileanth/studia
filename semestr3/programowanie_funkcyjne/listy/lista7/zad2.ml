



type 'a t = int -> 'a * int


let rand_val s =
  16807 * (s mod 127773) - 2836 * (s / 127773)

let gen_seed s =
  let z = rand_val s in
  if z > 0 then z else z + 2147483647

let return (x : 'a) = fun (s : int) -> x, s

let run (m : 'a t) (s : int) = m s |> fst

let rand s = (rand_val s, gen_seed s)

let bind (x : 'a t) (f : 'a -> 'b t) :'b t =
  fun s -> 
    let (v, ns) = x s in
    f v ns


let rec rem xs n =
  if n = 0 then List.tl xs else List.hd xs :: (rem (List.tl xs) (n-1))

let ( let* ) = bind


let rec shuffle (xs : 'a list) (acc : 'a list) : 'a list t =
  match xs with
  | [] -> (return acc)
  | y :: ys -> 
    let* r = rand in 
    let i = (r mod (List.length xs)) in 
       shuffle (rem xs i) (List.nth xs i :: acc) 


