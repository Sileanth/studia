

let make_set (type s) cmp =
  let module S = Set.Make(struct
  type t = s
  let compare = cmp
  end) in
  (module S : Set.S with type elt = s)




let is_generated (type a) (module Per : Perm.S with type t = a) (perm : a) (gen : a list) = 
  let helper (module Q : Set.S with type elt = a) = 
    let x0 = Q.of_list gen in
    let rec rek (prev : Q.t) =
      let inv = Q.map Per.invert prev in
      let product = Q.fold 
        (fun (a : a) (s: Q.t) -> 
          let row = Q.fold 
            (fun (b : a) (s : Q.t) ->
              Q.union (Q.singleton (Per.compose a b)) s
            )
            prev
            Q.empty
          in Q.union s row
        )
        prev
        Q.empty 
      in let succ = prev |> Q.union inv |> Q.union product in
      if Q.mem perm succ then true 
      else if Q.equal succ prev then false 
      else rek succ
    in rek x0
  in helper (make_set Per.compare)



  



module I = Perm.Make(Int)

let perm =  (module I : Perm.S with type t = I.t)


let z = I.swap 2 3
let y = I.swap 3 4
let x = I.swap 4 5

let v = I.invert (I.compose (I.compose z y) x)
let l = [z ; y ; x ]

let v = is_generated perm v l

let _ = if v then print_endline "sukces" else print_endline "porazka"