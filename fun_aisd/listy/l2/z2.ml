

module type Ordered = sig
  type t 
  val eq : t -> t -> bool
  val lt : t -> t -> bool 
  val le : t -> t -> bool 
end

module MakeHeap(Ord: Ordered) : sig 
  type t
  val rank : t -> int
  val makeHeap : Ord.t -> t -> t -> t
  val empty : t
  val isEmpty : t -> bool
  val merge : t -> t -> t
  val insert : Ord.t -> t -> t
  val findMin : t -> Ord.t option
  val delmin : t -> t option
  val fromList : Ord.t list -> t

end = struct 
  
  type elem = Ord.t
  let eq = Ord.eq
  let lt = Ord.lt 
  let le = Ord.le

  type t = Leaf | Node of int * elem * t * t 

  let rank = function 
  | Leaf -> 0 
  | Node(r, _, _, _) -> r 

  let singl x = Node(1, x, Leaf, Leaf)

  let makeHeap x a b = 
    let (ra, rb) = rank a, rank b in 
    if ra >= rb then
      Node(rb + 1, x, a, b)
    else 
      Node(ra + 1, x, b, a)

  let empty = Leaf

  let isEmpty = function
  | Leaf   -> true 
  | Node _ -> false

  let rec merge h1 h2 =
    match h1, h2 with 
    | h1, Leaf -> h1
    | Leaf, h2 -> h2 
    | Node(_, x, a1, b1), Node(_, y, a2, b2) -> 
      if le x y then 
        makeHeap x a1 (merge b1 h2)
      else
        makeHeap y a2 (merge h1 b2)

  let insert x h = 
    merge (singl x) h

  let findMin = function
  | Leaf -> None 
  | Node(_, x, _, _) -> Some(x)

  let delmin = function
  | Leaf -> None 
  | Node(_, _, a, b) -> Some(merge a b)

  let fromList xt =
    let hs = List.map singl xt in  
    let rec mergeRow ys acc = 
    match ys with 
    | [] -> acc
    | [h] -> h :: acc
    | a :: b :: ys -> mergeRow ys (merge a b :: acc)
    in let rec worker ys =
      match ys with 
      | [] -> Leaf
      | [h] -> h 
      | ys -> worker (mergeRow ys []) 
    in worker hs


  let minmax a b =
    if le a b then a,b else b,a
  
  let rec insertDirect h v = 
    match h with 
    | Leaf -> singl v 
    | Node(_, x, l, r) -> 
      let mi, ma = minmax x v in
      makeHeap mi l (insertDirect r ma)
  


end

module OrderedInt = struct 
  type t = int 
  let eq a b = a = b
  let lt a b = a < b 
  let le a b = a <= b
end

module IntHeap = MakeHeap(OrderedInt)
open IntHeap

let sort xs = 
  let h = fromList xs in 
  let rec helper h acc = 
    match findMin h, delmin h with 
    | None, _ -> acc 
    | _, None -> acc
    | Some i, Some h -> helper h (i :: acc) 
  in List.rev (helper h [])

