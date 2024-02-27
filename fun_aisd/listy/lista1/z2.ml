

let part x xs =
  let pred y = y <= x in
  List.partition pred xd

let rec quick_sort = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> 
      let (less, big) = part x xs in
      quick_sort less @ (x :: quick_sort big)
