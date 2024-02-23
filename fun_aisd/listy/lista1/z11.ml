



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