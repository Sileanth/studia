type (_, _) format = 
| Int :  (int -> 'a, 'a) format
| String : (string -> 'a, 'a) format
| Lit : string-> ('a, 'a) format
| Cat : ('a, 'b)  format * ('b, 'c) format -> ('a, 'c) format
   
let (^^) (i :  ('c, 'a) format) (o :  ('a, 'b) format) 
  = Cat (i, o)

let rec robol : type a b. (a, b) format -> (string -> b) -> string -> a =
  function 
  | Lit (l) -> fun (k : string -> b) (s : string) -> k (s ^ l)
  | Int -> (fun(k : string -> b) (s : string) -> (fun (i : int) -> k ( s ^ (string_of_int i) )))
  | String -> (fun (k : string -> b) (s : string) -> (fun (sn : string) -> k (s ^ sn)))
  | Cat (i, o) -> fun (k : string -> b) (s : string) -> robol i (robol o k) s

let ksprintf z k =
  robol z k ""

let sprintf z =
  ksprintf z (fun x -> x)

let conc_func (f : 'a -> 'b) (s : 'b -> 'c) =
  fun x -> s (f x)

let rec robcio : type a b. (a, b) format -> b -> a =
  function 
  | Lit l -> fun (k : b) -> print_string l; k
  | Int -> fun (k : b) (i : int) -> print_int i; k
  | String -> fun (k : b) (s : string) -> print_string s; k
  | Cat (i, o) ->  fun (k : b) -> robcio i (robcio o k) 



let printf z =
  robcio z  ()

let v = printf (Lit "abc" ^^ Int ^^ Lit "cda" )