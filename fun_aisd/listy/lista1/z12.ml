

type 'a array = 'a Array.t



type 'a hp_array_data = 
  | Current of ('a option) array
  | Past of (int * ('a option)) * 'a hp_array
and 'a hp_array = 'a hp_array_data ref 


let empty len = Current(Array.make len None)



let rec sub (hp_arr, id) = 
  match !hp_arr with
  | Current(arr) -> Array.get arr id 
  | Past((cid, cv), child) when cid = id -> cv
  | Past(_, child) -> sub (child, id)

let update (hp_arr, id, v) = 
  match !hp_arr with 
  | Past(_) -> failwith "only newest array can be updated"
  | Current(arr) ->  
    let prev = Array.get arr id in
    Array.set arr id (Some v);
    let new_cur = ref (Current(arr)) in
    hp_arr := Past((id, prev), new_cur);
    new_cur
