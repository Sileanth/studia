


type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Eith :'a fin_type * 'b fin_type -> (('a , 'b) Either.t) fin_type
| Func : 'a fin_type * 'b fin_type -> ('a -> 'b) fin_type

type (_, 'a) format = 
| Int : ((string -> 'a) -> string -> int -> 'a) -> (int -> 'a, 'a) format
| String : 'a -> (string -> 'a, 'a) format
| Lit : 'a -> ('a, 'a) format
| Concat : 


let int (k : string -> 'a) =
  fun (s : string) (i : int) ->
    k ( s ^(string_of_int i) )
  

let str (k : string -> 'a) =
  fun (s : string) (st : string) ->
    k ( s ^ st)

let lit sn =
  fun (k : string -> 'a) (s : string) ->
     k (s ^ sn)
   

let (^^) (f :  ('c, 'a) format) (s :  ('a, 'b) format) (k : (string -> 'b))  = 
    f (s k)