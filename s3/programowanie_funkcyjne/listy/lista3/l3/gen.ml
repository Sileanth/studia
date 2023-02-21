module type Permutation = sig
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

module type S = 
    sig
        type t
        val is_generated : t -> t list -> bool
    end

module Make(Perm : Permutation) =
struct 
  module S = Set.Make(Perm)
  type t = Perm.t
  let is_generated (perm : t) (gen : t list) =
    let x0 = List.fold_left (fun (s : S.t) (p : t) -> S.add p s) S.empty gen in
    let rec rek (prev : S.t) =
      let inv = S.fold (fun (a : t) (s : S.t) -> (S.add (Perm.invert a) s)) prev S.empty in
      let product = S.fold 
        (fun (a : t) (s: S.t) -> 
          let row = S.fold 
            (fun (b : t) (s : S.t) ->
              S.union (S.singleton (Perm.compose a b)) s
            )
            prev
            S.empty
          in S.union s row
        )
        prev
        S.empty 
      in let succ = prev |> S.union inv |> S.union product in
      if S.equal succ prev then false 
      else if S.mem perm succ then true 
      else rek succ
    in rek x0

end