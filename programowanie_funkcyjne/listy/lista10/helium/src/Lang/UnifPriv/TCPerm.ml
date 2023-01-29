type t =
  { fwd : TConst.t TConst.Map.t
  ; bck : TConst.t TConst.Map.t
  }

let id =
  { fwd = TConst.Map.empty
  ; bck = TConst.Map.empty
  }

let rev p =
  { fwd = p.bck
  ; bck = p.fwd
  }

let compose_map m1 m2 =
  TConst.Map.merge (fun x c1 c2 ->
    match c2 with
    | None   -> c1
    | Some y ->
      begin match TConst.Map.find_opt y m1 with
      | None   -> Some y
      | Some z ->
        if TConst.equal x z then None
        else Some z
      end) m1 m2

let compose p1 p2 =
  { fwd = compose_map p1.fwd p2.fwd
  ; bck = compose_map p2.bck p1.bck
  }

let swap x y =
  if TConst.equal x y then id
  else
    let map = TConst.Map.add x y (TConst.Map.singleton y x) in
    { fwd = map
    ; bck = map
    }

let apply perm x =
  match TConst.Map.find_opt x perm.fwd with
  | None   -> x
  | Some y -> y

let image_of perm set =
  TConst.Set.map (apply perm) set

let carrier perm =
  perm.fwd
  |> TConst.Map.bindings
  |> List.map (fun (x, y) ->
      assert (not (TConst.equal x y));
      x)
  |> TConst.Set.of_list
