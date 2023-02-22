
open Kopiec
open Printf


let sort xs = 
  let rec odkop k = 
    if empty k then [] else value k :: odkop (pop k) in
  odkop (List.fold_left (fun x y -> add y x) null xs)

let rec rand_list = function
| 0 -> []
| x -> (Random.int 100) :: rand_list (x - 1)


let l = rand_list 10


let () = List.iter (printf "%d ") l
let () = (printf "abc \n")
let () = List.iter (printf "%d ") (sort l)
