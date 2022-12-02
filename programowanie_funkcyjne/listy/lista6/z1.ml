

type ('a, 'b) format = (string -> 'b) -> (string -> 'a)



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


let sprintf (z : ('a, string) format) =
  z (fun s -> s) ""

let z =sprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit "." ^^ lit "abc" ^^ int) ;;

let s = z 12 "ów" 13