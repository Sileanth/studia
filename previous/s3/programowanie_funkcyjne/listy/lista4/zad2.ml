

type 'a zlist =
 |Zlist of 'a list * 'a list



let of_list (xs : 'a list) =
  Zlist ([], xs)

let to_list (zs : 'a zlist) =
  match zs with
  | Zlist (c, xs) -> List.rev c @ xs

let elem zs =
  match zs with
  | Zlist (_, []) -> None
  | Zlist (_, (x :: xs)) -> Some x

let move_left zs =
  match zs with
  | Zlist ([], ys) -> Zlist ([], ys)
  | Zlist (x :: xs, ys) -> Zlist (xs , x :: ys)

let move_right zs =
  match zs with
  | Zlist (xs, []) -> Zlist (xs, [])
  | Zlist (xs, y :: ys) -> Zlist (y :: xs ,ys)

let insert z zs =
  match zs with
  | Zlist (xs, ys) -> Zlist(xs, z :: ys)

let remove zs =
  match zs with
  | Zlist (xs, []) -> Zlist (xs, [])
  | Zlist (xs, (y:: ys)) -> Zlist(xs, ys)  

  