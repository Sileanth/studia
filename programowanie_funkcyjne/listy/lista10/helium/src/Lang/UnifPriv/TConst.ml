type mpath = string list
type info = mpath * Kind.t

include Utils.Variable.Make(struct
  type typ = info
  let default_name = "T"
end)
let kind  x = snd (type_of x)
let mpath x = fst (type_of x)

let full_name x =
  List.fold_right (fun m name ->
      if name = "this" then m
      else m ^ "." ^ name
    ) (mpath x) (name x)
