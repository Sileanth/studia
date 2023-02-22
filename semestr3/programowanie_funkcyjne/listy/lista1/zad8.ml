type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let ctrue = {cbool = fun f s -> f}
let cfalse = {cbool = fun f s -> s}


let cand b1 b2 = {cbool = fun x y -> (b1.cbool b2.cbool b1.cbool) x y}
let cor b1 b2 = {cbool = fun x y ->  (b1.cbool b1.cbool b2.cbool) x y }

let cbool b = if b then ctrue else cfalse
let bool_of_cbool f = f.cbool true false

let zer = {cnum = fun f x -> x}
let succ p = {cnum = fun f x -> f (p.cnum f x) }

let add a b = {cnum = fun f x ->  a.cnum f (b.cnum f x)  }

let mult a b ={cnum = fun f x -> a.cnum (b.cnum f) x } 


let is_zero n = n.cnum (fun x -> cfalse) ctrue 

let rec rek n f x = if n = 0 then x else f (rek (n - 1) f x) 
let cnum_of_int n = { cnum = fun f x -> rek n f x}

let int_of_cnum n = n.cnum (fun x -> x + 1) 0