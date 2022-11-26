

type 'a mylazy_data = 
| Lazy of (unit -> 'a)
| Busy
| Ready of 'a

type 'a mylazy = 'a ref

let force (l : 'a mylazy) =
  match !l with
  | Ready v -> v
  | Busy -> failwith "nie przeszkadzaj, pracuje"
  | Lazy f ->  l := Busy; let res = f () in l := (Ready res); res 
  
let fix (f : 'a mylazy -> 'a) : 'a mylazy =
  let res = ref Busy in
  
