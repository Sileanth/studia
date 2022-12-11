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
  type 'a t = s -> 'a sbt_list
  and 'a sbt_list =
    | Nil
    | Cons of 'a * s * 'a t 
  
  let fail = fun s -> Nil
  
  let return x = 
    fun s -> Cons (x, s, fail)
 
  let get =
    fun s -> Cons (s, s, fail)

  let put x =
    fun s -> Cons ((), x, fail)

  let flip = 
    fun s -> Cons (true, s, fun s -> Cons (false, s, fail))

  let rec run s m =
    match m s with
    | Nil -> Seq.empty
    | Cons (a, s, m) -> Seq.cons a (run s m)


   (* funkcja concatM jest wywoławana tylko z z niepustym xs, więc ignor warningi *)
  let rec concatM (xs : 'a sbt_list) (ms : 'a t) =
    let rec help xs (Cons (a, s, m)) =
      match xs with
      | Nil -> Cons (a, s, ms)
      | Cons (a, s, m) -> Cons (a, s, fun ns -> help (m ns) xs)
    in let Cons (a, s, n) = xs in
    help (n s) xs

  let rec bind (m : 'a t) (f : 'a -> 'b t) =
    fun s -> 
      match m s with
      | Nil -> Nil
      | Cons (a, sa, ma) ->
          match (f a sa) with
          | Nil -> bind ma f sa
          | Cons (b, sb, mb) -> concatM (Cons (b, sb, mb)) (bind ma f) 

end
