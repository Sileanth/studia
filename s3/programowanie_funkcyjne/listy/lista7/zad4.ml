


module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err: sig
  include Monad
  val fail : 'a t
  val run : 'a t -> 'a option 
  val catch : 'a t -> (unit -> 'a t) -> 'a t
end = 
struct
  type 'r ans = 'r option 
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans}

  let fail = {run = (fun k -> None)}

  let return x = {run = fun k -> k x}

  let catch m f =
      match m.run (fun a -> Some a)  with 
      | None -> f ()
      | Some a -> return a

  let bind m f =
    { run = fun k ->
      match m.run (fun a -> Some (f a)) with
      | None -> None
      | Some x -> x.run k
    }
  let run m : 'a option =
    m.run (fun a -> Some a)
end
module BT : sig
  include Monad
  val fail : 'a t
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct
  type 'r ans = 'r Seq.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans}
  let return x = {run = fun k -> k x}
  let fail = {run = fun k -> Seq.empty}
  let flip = {run = fun k -> Seq.concat_map k (List.to_seq [true ; false])}
  let run (m : 'a t) = m.run (fun a -> Seq.return a)
  let bind m (f : 'a -> 'b t) =
    { run = fun k ->
      let seq = m.run (fun a -> Seq.return a) in
      let z = Seq.concat_map (fun a -> (f a).run (fun z -> Seq.return z )) seq in
      Seq.concat_map (fun b -> k b) z
    }
end


module Make(State : sig type t end) : sig
  include Monad
  val get : State.t t
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a
end = struct
  type s = State.t
  type 'r ans = s -> 'r
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans}
  
  let return x = {run = fun k -> k x}

  let set s = {run = fun k -> 
    fun old_s -> k () s}
   
  let get = {run = fun k ->
    fun s -> k s s}
  
  let run (s : s) (m : 'a t) =
    m.run (fun a -> fun s -> a) s
  
  let bind (m : 'a t) (f : 'a -> 'b t)  =
    {run = fun k -> 
      (m.run (fun a -> (f a).run k))
    }

end








