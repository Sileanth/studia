open List


let rec maks cmp x xs =
  fold_left 
    (fun acc x -> if x > acc then x else acc)
    x
    xs

let rec mini cmp x xs =
  fold_left 
    (fun acc x -> if x < acc then x else acc)
    x
    xs


let rec makss cmp ub lb xs = 
  fold_left
    (fun acc x -> if x < ub && x > acc then x else acc)
    lb
    xs

let selsort cmp xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | x :: xss ->
      let ma = maks cmp x xs in
      let mi = mini cmp x xs in
      let rec helper xs maxi =
        let bmax = makss cmp maxi mi xs in
        if bmax = mi then [mi] 
        else bmax :: (helper xs bmax) 
      in ma :: (helper xs ma)

