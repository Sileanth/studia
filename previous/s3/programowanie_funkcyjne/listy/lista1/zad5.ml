
let hd x= x 0

let tail x = fun y -> x (y + 1)

let add s x = fun y -> x + s y

let map s f = fun x -> f (s x)

let map s1 s2 f = fun x -> f (s1 x) (s2 x)

let replace n a s = fun x -> if x = n then a else s x

let take_every n s = fun x -> s (n * x)

let rec inc x = if x = 0 then 0 else 1 + inc (x - 1)


let rec scan f s a = fun x -> if x = 0 then f a (s 0) else f (s (x - 1)) (s x)

let rec tabulate s ?(l=0) r = if l > r then [] else (s l) :: tabulate s ~l:(l+1) r