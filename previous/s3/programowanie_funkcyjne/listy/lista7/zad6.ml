
 module Make(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t 
end = struct
  type s = State.t
  type 'a t = s -> ('a * s) Seq.t  
  
  let return x : 'a t  = fun s ->
    Seq.return (x, s)

  let fail : 'a t = fun s -> Seq.empty
  
  let flip : bool t = 
    fun s -> List.to_seq [(true, s) ; (false, s)]
  
  let get : s t = 
    fun s -> Seq.return (s, s)
  
  let put x : unit t =
    fun s -> Seq.return ((), x)

  let run s (m : 'a t) =
    Seq.map fst (m s)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun s -> Seq.concat_map (fun (a,s) -> f a s) (m s) 
end
