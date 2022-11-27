type 'a dllist = 'a dllist_data Lazy.t
(* Note: lazy_t is the built-in type constructor used by the compiler for the lazy keyword. You should not use it directly. Always use Lazy.t instead. *)
and 'a dllist_data ={ prev : 'a dllist; elem : 'a; next : 'a dllist }

let prev (prev : 'a dllist) = 
  match prev with
| lazy {prev; elem; next} -> prev


let next (prev : 'a dllist) = 
  match prev with
  | lazy {prev; elem; next} -> next

let elem (prev : 'a dllist) = 
  match prev with
  | lazy {prev; elem; next} -> elem





let rec left v p =
  let rec g = lazy {prev = left (v-1) g ; elem = v; next = p} in g

let rec right v p=
  let rec g = lazy {prev = p; elem = v; next = right (v+1) g} in g

let rec integers = lazy {prev = left (-1) integers; elem = 0; next = right 1 integers}
  