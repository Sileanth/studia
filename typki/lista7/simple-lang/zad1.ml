(** Type inference (for simple types) *)

 
type tvar = string

  type uvar = t option ref
  and st = 
    | Stype of t
    | SForAll of tvar
  and t =
    | TUnit
    | TEmpty
    | TBool
    | TVar of tvar
    | TInt
    | TUVar   of uvar
    | TArrow  of t * t


  let fresh_uvar () = TUVar (ref None)

  let t_unit  = TUnit
  let t_empty = TEmpty
  let t_bool  = TBool
  let t_int   = TInt
  let t_arrow  tp1 tp2 = TArrow(tp1, tp2)

  let rec view tp =
    match tp with
    | TUVar x ->
      begin match !x with
      | None -> tp
      | Some tp ->
        let tp = view tp in
        x := Some tp;
        tp
      end
    | _ -> tp

  let set_uvar x tp =
    match !x with
    | None -> x := Some tp
    | Some _ -> assert false

