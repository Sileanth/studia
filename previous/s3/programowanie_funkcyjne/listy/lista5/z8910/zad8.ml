

type empty  = |


type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Empty : empty fin_type
| Eith :'a fin_type * 'b fin_type -> (('a , 'b) Either.t) fin_type
| Func : 'a fin_type * 'b fin_type -> ('a -> 'b) fin_type




let gen_func (c : 'a Seq.t) (d : 'b Seq.t) =
  let fun_list = Clist.fun_gen (List.of_seq d) (List.of_seq c)
  in List.to_seq (List.map (fun x -> 
      (fun z -> (snd (List.find (fun (a, b) -> a = z) x)))
    ) fun_list)

    
let rec all_values : type a. a fin_type -> a Seq.t = function
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair (c, d) -> Seq.product (all_values c) (all_values d)
  | Empty -> Seq.empty
  | Eith (c, d) -> Seq.append 
    (Seq.map (fun x-> Either.Left x) (all_values c)) 
    (Seq.map (fun x-> Either.Right x) (all_values d))
  | Func (c, d) -> gen_func (all_values c) (all_values d)


