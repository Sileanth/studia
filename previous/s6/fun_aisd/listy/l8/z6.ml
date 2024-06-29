module Sortable(T : sig 
  type t
  val compare : t -> t -> int
end) = struct
  type elem = T.t
  let compare = T.compare 

  type t = int * (elem Seq.t) list

  let empty = (0, [])

open Seq

  let rec merge (l1 : elem Seq.t) (l2: elem Seq.t) = match Seq.uncons l1, Seq.uncons l2 with
    | None, _ -> l2
    | _, None -> l1
    | Some (x, xs), Some (y, ys) -> 
      if T.compare x y <= 0 then fun () -> Cons(x, (merge xs l2))
      else fun () -> Cons(y, (merge l1 ys))
     

  let add x (n, l) = 
    let rec addSeg (seg: elem Seq.t) (segs: elem Seq.t list) (size: int) = 
      if size mod 2 = 0 then seg :: segs
      else addSeg (merge seg (List.hd segs)) (List.tl segs) (size / 2)
    in (n + 1, addSeg (fun () -> Cons(x, Seq.empty)) l n)

  let rec mergeAll (acc : elem Seq.t) (ys: elem Seq.t list) = match acc, ys with
    | acc, [] -> acc
    | acc, x :: xs -> mergeAll (merge acc x) xs


  let rec sort (n, l) = 
    let rec toList (s: elem Seq.t) = match Seq.uncons s with
      | None -> []
      | Some (x, xs) -> x :: toList xs
    in toList (mergeAll Seq.empty l)

  let rec takeK (k: int) (n, segs) = 
    let rec rek (licz: int) (s: elem Seq.t) =
      if licz = 0 then []
      else match Seq.uncons s with
        | None -> []
        | Some (x, xs) -> x :: rek (licz - 1) xs
    in rek k (mergeAll Seq.empty segs)


  end