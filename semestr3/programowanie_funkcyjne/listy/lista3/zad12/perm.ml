module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
    type key
    type t
    (** permutacja jako funkcja *)
    val apply : t -> key -> key
    (** permutacja identycznościowa *)
    val id : t
    (** permutacja odwrotna *)
    val invert : t -> t
    (** permutacja która tylko zamienia dwa elementy miejscami *)
    val swap : key -> key -> t
    (** złożenie permutacji (jako złożenie funkcji) *)
    val compose : t -> t -> t
    (** porównywanie permutacji *)
    val compare : t -> t -> int
end

module Make(Key : OrderedType) =
struct
  module MyMap = Map.Make(Key)
  type key = Key.t
  type t = {p : key MyMap.t; r : key MyMap.t}
  

  let (id : t) ={ p = MyMap.empty ; r= MyMap.empty}

  let apply (perm : t) (el : Key.t) =
    match (MyMap.find_opt el perm.p) with
    | None -> el
    | Some x -> x

  let invert (perm : t) =
    match perm with
    | {p = a; r = b} -> {p = b; r = a}

  let swap (a : key) (b : key) =
    if Key.compare a b = 0 
      then id 
    else 
      let p = (MyMap.add b a (MyMap.add a b MyMap.empty)) in
      let r = (MyMap.add b a (MyMap.add a b MyMap.empty))  in
      {p = p; r = r}

    let compose (a : t) (b : t) =
      let val_compose (k : key) (x : key option) (y : key option) =
        match x, y with
        | None, None -> None
        | None, Some z -> Some z
        | Some z, _ -> let res = apply b z in 
          if res = k 
            then None 
            else Some res
      in
      let val_compose_r (k : key) (x : key option) (y : key option) =
        match x, y with
        | None, None -> None
        | Some z , None -> Some z
        | _, Some z -> let res = apply (invert a) z in 
          if res = k 
            then None 
            else Some res
      in 
      { p = MyMap.merge val_compose a.p b.p ; r= MyMap.merge val_compose_r a.r b.r}


      let compare (a : t) (b : t) =
        MyMap.compare Key.compare a.p b.p



end