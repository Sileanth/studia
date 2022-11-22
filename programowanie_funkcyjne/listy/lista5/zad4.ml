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
  | lazy {prev; elem; next} -> Lazy.force elem


let of_list (xs : 'a list) =
  let rec rek first prev xs =
    match xs with 
    | [] -> first 
    | x :: xs -> let rec node = lazy ({prev = prev; elem = x; next =  (rek first node xs) }) in
      lazy (Lazy.force node )
  in let rec first = rek first first xs  
in first
    
