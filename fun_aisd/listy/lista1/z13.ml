

type tree = Leaf | Node of int * tree * tree


let rec member x = function
  | Leaf -> false
  | Node(y, tl, tr) ->
      if x <= y then
        if y <= x then true
        else member x tl
      else member x tr

let rec insert x t = 
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node(y, tl, tr) ->
      if x <= y then
        if y <= y then t 
        else Node(y, insert x tl, tr)
      else Node(y, tl, insert x tr)

let rec mem2 x t =
  match t with
  | Leaf -> false 
  | Node(y, _, _) ->
      let rec help c = function 
        | Leaf -> c = x 
        | Node(y, a, b) ->
            if x < y then help c a
            else help y b
      in help y t 


exception Same
let rec ins2 x t =
  let rec helper c = function 
    | Leaf -> if c = x 
            then raise Same 
            else Node(x, Leaf, Leaf)
    | Node(y, a, b) -> 
        if x < y 
        then Node(y, helper c a, b)
        else Node(y, a, helper y b)
  in try helper x t
  with Same -> t

let ins3 x t =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node(y, _, _) ->
      let rec helper c t = 
        match t with
        | Leaf when c = x -> raise Same
        | Leaf -> Node(x, Leaf, Leaf)
        | Node(y, a, b) ->
            if x < y 
            then Node(y, helper c a, b)
            else Node(y, a, helper y b)
      in try helper y t 
      with Same -> t 
