



module Tablica : sig 
 
  type 'a array
  val empty : 'a array
  val sub : 'a array * int -> 'a option
  val update : 'a array * int * 'a -> 'a array

  
  
end = struct
  module M = Map.Make(Int)
  type key = int

  type 'a array = 'a M.t

  let empty = M.empty

  let sub (arr, id) = 
    M.find_opt id arr

  let update (arr, id, v) =
    M.add id v arr 

end

module TablicaWeird = struct
  type 'a table = Leaf 
  | Node of 'a option * 'a table * 'a table 

  let empty = Leaf 
  let rec sub x = function 
    | Leaf -> None
    | Node(oa, _, _) when x = 0 -> oa 
    | Node(_, l, _) when x mod 2 = 0 -> sub (x /2) l
    | Node(_, _, r) -> sub (x / 2) r

  let rec update id x = function 
    | Leaf when id = 0 -> Node(Some x, Leaf, Leaf)
    | Leaf when id mod 2 = 0 -> 
        Node(None, update (id / 2) x Leaf, Leaf)
    | Leaf -> Node(None, Leaf, update (id / 2) x Leaf)
    | Node(y, l, r) when id = 0 -> Node(Some x, l, r)
    | Node(y, l, r) when id mod 2 = 0 ->
        Node(y, update (id / 2) x l, r)
    | Node(y, l, r)  ->
        Node(y, l, update (id / 2) x r)



end
