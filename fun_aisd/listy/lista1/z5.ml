



let rec split xs =
    let rec helper xs l r =
        match xs with
        | []             -> (l, r)
        | [x]            -> (x :: l, r)
        | x :: (y :: zs) -> helper zs (x :: l) (y :: r)
    in helper xs [] []

let rec merge xs ys =
match xs, ys with
| [], ys -> ys
| xs, [] -> xs
| x :: xss, y :: yss when x <= y -> x :: (merge xss ys)
| xs, y :: yss -> y :: (merge xs yss)


(*
    czas i consy
    mamy O(log n) pięter, bo w każym kroku połowimy liste
    oraz na każdym piętrze merge i split wykonują sumarycznie O(n) operacji
    czyli czas działania i liczna consów to O(log n)

    żywa pamięć
    na 1 wywołaniu rekurencji mamy n żywych komórek 
    na 2 mamy n/2 + pierwsze piętro

    czyli maksymalne zużycie pamięci nie przekracza 2n z własności szeregu geometrycznego

*)
let rec merge_sort = function
| []  -> []
| [x] -> [x]
| xs  -> 
    let (l, r) = split xs in
    merge (merge_sort l) (merge_sort r)