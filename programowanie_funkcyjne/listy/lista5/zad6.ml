

type 'a mylazy_data = 
| Lazy of (unit -> 'a)
| Busy
| Ready of 'a

type 'a mylazy = 'a mylazy_data ref

let force (l : 'a mylazy) =
  match !l with
  | Ready v -> v
  | Busy -> failwith "nie przeszkadzaj, pracuje"
  | Lazy f ->  l := Busy; let res = f () in l := (Ready res); res 
  
let fix (f : 'a mylazy -> 'a) : 'a mylazy =
  let (rek :'a mylazy) = ref Busy in
  rek := Lazy (fun () -> (f rek));  rek


type 'a lazy_list =
  | Cons of 'a * 'a lazy_list mylazy 


let head (ls : 'a lazy_list) =
  match ls with
  | Cons (l, ls) -> l

let tail (ls : 'a lazy_list) =
  match ls with
  | Cons (l, ls) -> force ls


let stream_of_ones = fix (fun stream_of_ones -> Cons(1, stream_of_ones))


let nat_stream =
  let rec succ x =
    fun nat_stream -> Cons (x, fix (succ (x+1))) in
  fix (succ 1)