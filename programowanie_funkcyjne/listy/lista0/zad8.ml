let f x y = x in
let error a = (7/0) + a in
f 5 (error 3)
