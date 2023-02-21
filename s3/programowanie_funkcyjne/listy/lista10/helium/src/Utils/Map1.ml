open EqDec

module type UIDType1 = sig
  type 'a t

  val uid : 'a t -> UID.t

  val gequal : 'a t -> 'b t -> ('a, 'b) eq_dec
end

module type TypeFamily1 = sig
  type 'a t
end

module type FS = sig
  type 'a key
  type 'a v
  type t

  val empty : t
  val singleton : 'a key -> 'a v -> t
  val add : 'a key -> 'a v -> t -> t

  val remove : 'a key -> t -> t

  val mem : 'a key -> t -> bool
  val find : 'a key -> t -> 'a v
  val find_opt : 'a key -> t -> 'a v option
end

module type S = sig
  type 'a key
  type 'v t

  val empty : 'v t
  val singleton : 'a key -> 'v -> 'v t
  val add : 'a key -> 'v -> 'v t -> 'v t
  
  val remove : 'a key -> 'v t -> 'v t

  val mem : 'a key -> 'v t -> bool
  val find : 'a key -> 'v t -> 'v
  val find_opt : 'a key -> 'v t -> 'v option

  module type S = FS with type 'a key := 'a key
  module Make(V : TypeFamily1) : S with type 'a v = 'a V.t
end

module Make(Key : UIDType1) : S with type 'a key = 'a Key.t = struct
  type 'a key = 'a Key.t
  type 'v v =
  | Val : 'a key * 'v -> 'v v
  type 'v t = 'v v UID.Map.t

  let empty = UID.Map.empty

  let singleton x v =
    UID.Map.singleton (Key.uid x) (Val(x, v))

  let add x v m =
    UID.Map.add (Key.uid x) (Val(x, v)) m

  let remove x m =
    UID.Map.remove (Key.uid x) m

  let mem x m =
    UID.Map.mem (Key.uid x) m

  let find x m =
    let (Val(_, v)) = UID.Map.find (Key.uid x) m in v

  let find_opt x m =
    match UID.Map.find_opt (Key.uid x) m with
    | None -> None
    | Some(Val(_, v)) -> Some v

  module type S = FS with type 'a key := 'a key

  module Make(V : TypeFamily1) : S with type 'a v = 'a V.t = struct
    type 'a v = 'a V.t
    type vv =
    | Val : 'a key * 'a v -> vv

    type t = vv UID.Map.t

    let empty = UID.Map.empty

    let singleton x v =
      UID.Map.singleton (Key.uid x) (Val(x, v))

    let add x v m =
      UID.Map.add (Key.uid x) (Val(x, v)) m

    let remove x m =
      UID.Map.remove (Key.uid x) m

    let mem x m =
      UID.Map.mem (Key.uid x) m

    let find (type a) (x : a key) m : a v =
      let (Val(y, v)) = UID.Map.find (Key.uid x) m in
      match Key.gequal x y with
      | Equal -> v
      | NotEqual -> assert false (* UID is not unique *)

    let find_opt (type a) (x : a key) m : a v option =
      match UID.Map.find_opt (Key.uid x) m with
      | None -> None
      | Some(Val(y, v)) ->
        begin match Key.gequal x y with
        | Equal    -> Some v
        | NotEqual -> assert false (* UID is not unique *)
        end
  end
end
