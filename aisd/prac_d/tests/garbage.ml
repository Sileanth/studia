


    

    type tree =
      | Empty
      | Leaf of int 
      | Node2 of  int * int  * int * tree * tree 
      | Node3 of  int * int * int * tree * tree * tree


    let empty = Empty 


    let height = function
      | Leaf _ -> 0
      | Node2 (h, _, _, _, _) -> h 
      | Node3 (h, _, _, _, _, _) -> h

    let safe_height = function
      | Leaf _ -> Some 0
      | Node2 (h, _, _, _, _) -> Some h 
      | Node3 (h, _, _, _, _, _) -> Some h
      | Empty -> None

  let safe_smallest = function 
      | Leaf x -> Some x 
      | Node2 (_, x, _, _, _) -> Some x 
      | Node3 (_, x, _, _, _, _) -> Some x
      | Empty -> None
    



    let smallest = function 
      | Leaf x -> x 
      | Node2 (_, x, _, _, _) -> x 
      | Node3 (_, x, _, _, _, _) -> x
    
    let biggest = function 
      | Leaf x -> x 
      | Node2 (_, _, x, _, _) -> x 
      | Node3 (_, _, x, _, _, _) -> x

    let safe_biggest = function 
      | Leaf x -> Some x 
      | Node2 (_, _, x, _, _) -> Some x 
      | Node3 (_, _, x, _, _, _) -> Some x
      | Empty -> None


    let node2 a b =
      Node2 (height a + 1, smallest a, biggest b, a, b)

    let node3 a b c =
      Node3 (height a + 1, smallest a, biggest c, a, b, c)

    let levelUp = function 
      | [a ; b] -> [node2 a b]
      | [a ; b ; c] -> [node3 a b c]
      | [a ; b ; c ; d] -> [node2 a b ; node2 c d]

    let rec mergeToSameHeight a b = 
      match compare (height a) (height b) with 
      | -1 -> 
          begin match b with 
          | Node2 (_, _, _, b1, b2) -> levelUp (mergeToSameHeight a b1 @ [b2])
          | Node3 (_, _, _, b1, b2, b3) -> levelUp (mergeToSameHeight a b1 @ [b2 ; b3])
          end
      | 1 -> 
          begin match a with 
          | Node2 (_, _, _, a1, a2) -> levelUp ([a1] @ mergeToSameHeight a2 b)
          | Node3 (_, _, _, a1, a2, a3) -> levelUp ([a1; a2] @ mergeToSameHeight a3 b)
          end 
      | _ -> [a; b]
      


  let merge a b =
    match a,b with
    | a, Empty -> a 
    | Empty, b -> b 
    | a,b -> 
      begin match mergeToSameHeight a b with 
      | [x] -> x 
      | [x ; y] -> node2 x y 
      end

  type g_criterium = 
    | Greater of int 
    | GreaterEqual of int

  type l_criterium =
    | Lesser of int 
    | LesserEqual of int 

  let resolve_g crit x =
    match crit with
    | Greater y -> x > y 
    | GreaterEqual y -> x >= y

  let resolve_l crit x =
    match crit with
    | Lesser y -> x < y
    | LesserEqual y -> x <= y 

  let resolve_g_tree crit t = resolve_g crit (smallest t)
  
  let resolve_l_tree crit t = resolve_l crit (biggest t)


  let rec split_g crit = function 
    | Empty -> (Empty, Empty) 
    | Leaf x -> 
        begin match resolve_g crit x with
        | true -> (Empty, Leaf x)
        | false -> (Leaf x, Empty)
        end
    | Node2 (_, _, _, a, b) ->
        begin match resolve_g_tree crit b with 
        | true -> 
            let (a1, a2) = split_g crit a 
            in (a1, merge a2 b)
        | false -> 
            let (b1, b2) = split_g crit b 
            in (merge a b1, b2) 
        end 
    | Node3 (_, _, _, a, b, c) ->
        begin match a with 
        | a when resolve_g_tree crit b -> 
            let (a1, a2) = split_g crit a 
            in (a1, merge a2 (node2 b c))
        | a when resolve_g_tree crit c -> 
            let (b1, b2) = split_g crit b 
            in (merge a b1, merge b2 c)
        | a                           ->
            let (c1, c2) = split_g crit c 
            in (merge (node2 a b) c1, c2)
        end

 let rec split_l crit = function 
    | Empty -> (Empty, Empty) 
    | Leaf x -> 
        begin match resolve_l crit x with
        | true -> (Leaf x, Empty)
        | false -> (Empty, Leaf x)
        end
    | Node2 (_, _, _, a, b) ->
        begin match resolve_l_tree crit a with 
        | true -> 
            let (b1, b2) = split_l crit b 
            in (merge a b1, b2)
        | false -> 
            let (a1, a2) = split_l crit a 
            in (a1, merge a2 b) 
        end 
    | Node3 (_, _, _, a, b, c) ->
        begin match a with 
        | a when resolve_l_tree crit b -> 
            let (c1, c2) = split_l crit c 
            in (merge (node2 a b) c1, c2)
        | a when resolve_l_tree crit a -> 
            let (b1, b2) = split_l crit b 
            in (merge a b1, merge b2 c)
        | a                           ->
            let (a1, a2) = split_l crit a 
            in (a1, merge a2 (node2 b c))
        end




  let upper t x = 
    let (_, nt) = split_g (GreaterEqual x) t 
    in safe_smallest nt 
  
  let lower t x = 
    let (nt, _) = split_l (LesserEqual x) t 
    in safe_biggest nt 

  let delete t x = 
    let (a1, a2) = split_g (GreaterEqual x) t 
    in let (_, a3) = split_g (Greater x) t 
    in merge a1 a3 

  let insert t x = 
    let (a1, a2) = split_g (GreaterEqual x) t 
    in merge (merge a1 (Leaf x)) a2


  let member t x =
    let (_, nt) = split_g (GreaterEqual x) t
    in match safe_smallest nt with
      | None -> false 
      | Some y -> x = y 



