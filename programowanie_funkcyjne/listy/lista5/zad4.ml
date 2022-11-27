type 'a dllist = 'a dllist_data Lazy.t
(* Note: lazy_t is the built-in type constructor used by the compiler for the lazy keyword. You should not use it directly. Always use Lazy.t instead. *)
and 'a dllist_data ={ prev : 'a dllist; elem : 'a; next : 'a dllist }

let prev (prev : 'a dllist) = 
  match prev with
| lazy {prev; elem; next} -> prev


let next = function
  | lazy {prev; elem; next} -> next

let elem (prev : 'a dllist) = 
  match prev with
  | lazy {prev; elem; next} -> elem



let rec gen (prev : 'a dllist) (first : 'a dllist) xs =
  match xs with
  | [] -> (prev, first)
  | x :: xs -> 
    let rec rob () = let (a, b) = gen (rob () |> fst) first xs in
     lazy {prev = rob () |> fst; elem = x; next = a},b in
    rob ()

let from_lsit xs =
  match xs with
  | [] -> failwith "puuuuusto"
  | x :: xs -> 
    let rec first () = 
      let (a,b) = gen (first ()) (first ()) xs in
      lazy {prev = a; elem = x; next = b} in
      first ()


let rec left_int v p =
  let rec g = lazy {prev = left_int (v-1) g ; elem = v; next = p} in g

let rec right_int v p=
  let rec g = lazy {prev = p; elem = v; next = right_int (v+1) g} in g

let rec integers = lazy {prev = left_int (-1) integers; elem = 0; next = right_int 1 integers}
  