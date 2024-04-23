module type Heap = sig
  type t
  type elem
  val empty : t
  val insert : elem -> t -> t
  val find_min : t -> elem
  val delete_min : t -> t
end


module ExplicitSizeHeap(H: Heap) = struct
  type t = H.t * int
  type elem = H.elem

  let empty = (H.empty, 0)

  let insert x (h, n) = (H.insert x h, n + 1)

  let find_min (h, _) = H.find_min h

  let delete_min (h, n) = (H.delete_min h, n - 1)

  let size (_, n) = n
end