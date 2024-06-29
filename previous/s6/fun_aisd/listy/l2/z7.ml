module type Ordered = sig
  type t 
  val eq : t -> t -> bool
  val lt : t -> t -> bool 
  val le : t -> t -> bool 
end

module MakeHeap(Ord: Ordered) : sig 
 (*
      type t
  val rank : t -> int
  val makeHeap : Ord.t -> t -> t -> t
  val empty : t
  val isEmpty : t -> bool
  val merge : t -> t -> t
  val insert : Ord.t -> t -> t
  val findMin : t -> Ord.t option
  val delmin : t -> t option
 *)


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
    let tot = ra + rb + 1 in
    if ra >= rb then
      Node(tot, x, a, b)
    else 
      Node(tot, x, b, a)

  let empty = Leaf

  let isEmpty = function
  | Leaf   -> true 
  | Node _ -> false

  let rec merge h1 h2 =
    match h1, h2 with 
    | h1, Leaf -> h1
    | Leaf, h2 -> h2 
    | Node(_, x1, a1, b1), Node(_, x2, a2, b2) -> 
      let helper x l r1 r2 = 
        let r = merge r1 r2 in 
        let tot = rank r1 + rank r2 + 1 in 
        if rank l < tot then 
          Node(tot, x, r, l)
        else 
          Node(tot, x, l, r)
      in 
      if le x1 x2 then 
        helper x1 a1 b1 h2
      else
        helper x2 a2 b2 h1 

  let insert x h = 
    merge (singl x) h

  let findMin = function
  | Leaf -> None 
  | Node(_, x, _, _) -> Some(x)

  let delmin = function
  | Leaf -> None 
  | Node(_, _, a, b) -> Some(merge a b)


end