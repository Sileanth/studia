
module BT : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Brak wyniku *)
  val fail : 'a t
  (** Niedeterministyczny wybór -- zwraca true, a potem false *)
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  (* Obliczenie typu 'a to leniwa lista wszystkich możliwych wyników *)
  type 'a t = 'a Seq.t

  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

let (let* ) = BT.bind
let flip = BT.flip
let fail = BT.fail
let return = BT.return
let run = BT.run


type 'a regexp =
  | Eps
  | Lit of ('a -> bool)
  | Or of 'a regexp * 'a regexp
  | Cat of 'a regexp * 'a regexp
  | Star of 'a regexp


let ( +% ) r1 r2 = Or(r1, r2)
let ( *% ) r1 r2 = Cat(r1, r2)

let append a b =
  let* c = flip in
  if c then a
  else b

let rec match_regexp (reg : 'a regexp) (xs : 'a list) : 'a list option BT.t =
  match reg with
  | Eps -> return None 
  | Lit p -> begin match xs with
    | [] -> fail
    | x :: xs -> if p x then return (Some xs) else fail 
  end
  | Or (a, b) -> 
      append (match_regexp a xs) (match_regexp b xs)
  | Cat (a,b) -> begin 
      let* suf = match_regexp a xs in 
      match suf with
        | None -> match_regexp b xs
        | Some xa -> let* suf' = match_regexp b xa in
          return (match suf' with
          | None -> Some xa
          | Some res -> Some res)
  end
  | Star a -> begin
      let* suf = match_regexp (Or (a, Eps)) xs in
      match suf with
      | None -> return None
      | Some xa -> append (match_regexp (Star a) xa) (return (Some xa))
  end


let p = Star (Star (Lit ((<>) 'b')) +% (Lit ((=) 'b') *% Lit ((=) 'a')))

let z = match_regexp p ['c' ; 'b' ;'a' ;'c';'c';'b';'a';'a';'b';'b';'a']
let ans = List.of_seq (run z)
let pustak = match_regexp p []
let pusta_ans = List.of_seq (run pustak)


(* Ta implementacja ma tą wadę że powtarzają się wyniki *)




