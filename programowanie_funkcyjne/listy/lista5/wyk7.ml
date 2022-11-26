open Seq

let rec filter p xs = fun () ->
  match xs () with
  | Nil -> Nil
  | Cons(x, xs) when p x -> Cons(x, filter p xs)
  | Cons(_, xs) -> filter p xs ()

let rec take_while p xs = fun () ->
  match xs () with
  | Nil -> Nil
  | Cons(x, xs) when p x -> Cons(x, take_while p xs)
  | Cons _ -> Nil

let rec for_all p xs =
  match xs () with
  | Nil -> true
  | Cons(x, xs) ->
    p x && for_all p xs

let rec nth n xs =
  match n, xs () with
  | _, Nil -> failwith "nth"
  | 0, Cons(x, _) -> x
  | _, Cons(_, xs) -> nth (n-1) xs

let rec nats_from n () = Cons(n, nats_from (n+1))

let rec primes () =
  let is_prime n =
    primes
    |> take_while (fun p -> p * p <= n)
    |> for_all (fun p -> n mod p <> 0)
  in
  Cons(2, filter is_prime (nats_from 3))
