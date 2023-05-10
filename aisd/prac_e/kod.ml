

module MakeUf(Size : sig val size : int end) : sig 


end = struct 

  let uf = Array.init (Size.size + 1) (fun x -> x)
  let rec find x = 
  let px = Array.get uf x in 
    if x = px 
    then x 
    else 
      let fx = find px in
      Array.set uf x fx;
      fx


let union a b =
  let fa = find a in 
  let fb = find b in
  Array.set uf fa fb
end


let n = read_int ()
let m = read_int ()

let code y x = 
  y * n + x 

let decode v = 
  (v / n,  v mod n)

let input_island n m =  
  let rec helper y x acc =
    begin match y >= n with
    | true -> acc 
    | false -> 
        begin match x >= n with
        | true -> helper (y+1) x acc
        | false -> helper y (x+1) ((read_int (), code x y) :: acc)
        end
    end 
  in helper 0 0 []

module Uf = MakeUf(struct let size = (n*m) end)


let islands = List.sort (fun (h1,_) (h2,_) -> compare h2 h1)  (input_island n m)

let t = read_int ()
let rec input_queries t acc = 
  match t with
  | 0 -> acc 
  | t -> input_queries (t-1) (read_int :: acc)

let queries = input_queries t [] 

let rec solve islands queries ans =
  match islands, queries with 
  | _, [] -> () 
  | [], (q :: qs) -> print_int ans; solve [] qs ans
  | ((h, v) :: is), (q :: qs) when q >= h -> print_int ans; solve islands qs ans 
  | ((h, v) :: is), qs -> 
      let 


  pri





