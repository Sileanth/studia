type 'a clist = {clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z}
let cnil = {clist = fun f z -> z}
let ccons x cl =
  match cl with
  | {clist = cl} -> {clist = 
      fun f z -> f x (cl f z)
    }

let map fm cl =
  {clist  = fun f z ->
    let nf na nz = f (fm na) nz in
    cl.clist nf z   
  }
    
let append a b =
  match a , b with
  | {clist = a} , {clist = b} ->
      {clist = fun f z ->
        a f (b f z)
      }

let clist_to_list = function
| {clist = cl} -> cl (fun a z -> a :: z) []

let rec list_to_clist (xs : 'a list) =
  match xs with
  | [] -> cnil
  | x :: xs -> ccons x (list_to_clist xs)

let prod a b = 
  {clist = fun f c->
      let temp = {clist = fun f c ->
          (a.clist (fun x z -> append (map (fun y -> x , y) b) z) cnil).clist f c
      } in
      temp.clist f c
  }

let sing x = 
  ccons x cnil

  let pot b e =
    {clist = fun fout aout ->
      (e.clist 
        (fun i c ->
          (map (fun (h , t) -> ccons h t)
            
            (prod (prod (sing i) b) c)
          )
        )
        (sing cnil)
      ).clist fout aout
    
    }

let fun_gen d c =
  List.map (clist_to_list) (clist_to_list (pot (list_to_clist d) (list_to_clist c)))