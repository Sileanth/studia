

type symbol = string
type 'v term =
  | Var of 'v
  | Sym of symbol * 'v term list


let return v = Var v


let bind t sub = 
  let rec rek = function
    | Var v -> sub v
    | Sym (s , xt) ->
        Sym (s, List.map rek xt)
  in rek t
