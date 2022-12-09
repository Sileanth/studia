module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig 
include Monad
val fail : 'a t
val catch : 'a t -> (unit -> 'a t) -> 'a t
val run : 'a t -> 'a option
end =
struct
  type 'a t = (unit -> 'a) -> 'a
  
  let fail : 'a t = fun cont -> cont ()
  let return (x : 'a) : 'a t  = fun cont -> x

  let catch (exp : 'a t) (err : unit -> 'a t) : 'a t =
    fun cont ->
      exp (fun () ->err () cont)

  let bind m f =
    fun k ->
      let a = m k

  let run m =
    m ()


end