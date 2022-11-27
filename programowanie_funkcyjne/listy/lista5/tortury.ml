type 'a dllist = 'a dllist_data Lazy.t
(* Note: lazy_t is the built-in type constructor used by the compiler for the lazy keyword. You should not use it directly. Always use Lazy.t instead. *)
and 'a dllist_data ={ elem : 'a; next : 'a dllist; prev : 'a dllist;  }

let prev (prev : 'a dllist) = 
  match prev with
| lazy {prev; elem; next} -> prev


let next = function
  | lazy {prev; elem; next} -> next

let elem (prev : 'a dllist) = 
  match prev with
  | lazy {prev; elem; next} -> elem



let sing x =
  let rec s = lazy {next = s; elem = x; prev = s} in
  s

let rec gen (prev : 'a dllist) xs first =
  match xs with
  | [] -> first, prev
  | x :: xs -> 
    let rec z = lazy begin
        let (a,b) = gen (fst z) xs first in
        (lazy {prev = prev; elem = x; next = a}), b
    end in
    Lazy.force z




let rec cycle xs =
  match xs with
  | [] -> failwith "puuusto"
  | x :: xs -> let last = ref (sing x) in
    let rec first = lazy begin
      let (a,b) = gen first xs first in
      last := b; {prev = b; elem = x; next = a}
    end in first