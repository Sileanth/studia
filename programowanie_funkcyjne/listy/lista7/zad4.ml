module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig 
 
end =
struct
  type 'a t = (unit -> 'a) ->  'a
  
  let fail : 'a t = fun cont -> cont ()
  let return (x : 'a) : 'a t  = fun cont -> x

  let catch (exp : 'a t) (catch : unit -> 'a t) : 'a t =
    fun cont ->
      exp catch


end