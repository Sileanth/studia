


let () = print_endline "Hello, World!"


let rec map f = function
    | [] -> []
  | x :: xs -> f x :: map f xs


let _ = 
  let _ = map (fun x -> x) [] in
    print_endline "abc";
  print_endline "ad"
