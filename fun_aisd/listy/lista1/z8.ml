

let app xs  ys =
  let rec helper acc = function
    | [] -> []
    | x :: xs -> loop (x :: acc) xs
  in app ys (List.rev xs)
