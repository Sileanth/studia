let ex_falso x = absurd x

let rec loop x = loop x

let foo y = ex_falso (loop ())

in

foo
