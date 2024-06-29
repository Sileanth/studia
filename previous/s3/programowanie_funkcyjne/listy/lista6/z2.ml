type (_, _) format = 
| Int :  (int -> 'a, 'a) format
| String : (string -> 'a, 'a) format
| Lit : string-> ('a, 'a) format
| Cat : ('a, 'b)  format * ('b, 'c) format -> ('a, 'c) format
   
let (^^) (i :  ('c, 'a) format) (o :  ('a, 'b) format) 
  = Cat (i, o)

let rec helper : type a b. (a, b) format -> (string -> b) -> string -> a =
  function 
  | Lit l -> fun (k : string -> b) (s : string) -> k (s ^ l)
  | Int -> (fun(k : string -> b) (s : string) -> (fun (i : int) -> k ( s ^ (string_of_int i) )))
  | String -> (fun (k : string -> b) (s : string) -> (fun (sn : string) -> k (s ^ sn)))
  | Cat (i, o) -> fun (k : string -> b) (s : string) -> helper i (helper o k) s

let ksprintf z k =
  helper z k ""

let sprintf z =
  ksprintf z (fun x -> x)

let conc_func (f : 'a -> 'b) (s : 'b -> 'c) =
  fun x -> s (f x)

let rec help2 : type a b. (a, b) format-> (unit -> b) -> (unit -> a) =
  function 
  | Lit l -> fun  (k : unit -> b) () -> print_string l; k ()
  | Int -> fun  (k : unit -> b) ()  (i : int) ->  print_int i; k ()
  | String -> fun  (k : unit -> b) ()  (s : string) -> print_string s; k ()
  | Cat (i, o) -> fun   (k : unit -> b) ()-> help2 i (help2 o k ) ()

let kprintf z k =
  help2 z (fun () -> k) () 

let printf z =
  kprintf z () 

let _ = printf (Int ^^ Lit " abc " ^^ Int ^^ Lit " cda " ^^ String ) 13 12 "hd"