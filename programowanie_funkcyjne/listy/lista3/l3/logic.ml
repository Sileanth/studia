type formula =
  | Var of string
  | Neg 
  | Imp of formula * formula
let string_of_formula f =
  let naw s =
    "(" ^ s ^ ")" in
  let rec rek f l =
    match f with
    | Var s -> s
    | Neg -> "⊥"
    | Imp (a, b) -> let x = rek a true ^ " -> " ^ rek b false in 
      if l then naw x else x in
  rek f false

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = 
  | Ax of {a : formula list; f : formula}
  | ImpI of {a : formula list; f : formula; x : theorem}
  | ImpE of {a : formula list; f : formula; x : theorem; y : theorem}
  | NegE of {a : formula list; f : formula; x : theorem}

let assumptions (thm : theorem) =
  match thm with
  | Ax {a} -> a
  | ImpI {a} -> a
  | ImpE {a} -> a  
  | NegE {a} -> a    
  

let consequence thm =
  match thm with
  | Ax {f} -> f
  | ImpI {f} -> f
  | ImpE {f} -> f  
  | NegE {f} -> f 

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
  Ax {a = [f]; f = f}

exception RulesErrors of string
let rec rem xs f = 
  match xs with
  | [] -> []
  | (x :: xs) when x = f -> xs
  | (x :: xs) -> x :: rem xs f

let sum xs ys =
  xs @ List.filter (fun x -> not (List.mem x xs)) ys

let imp_i f thm =
  ImpI {x = thm; f = Imp (f, consequence thm); a = (rem (assumptions thm) f)}

let imp_e th1 th2 =
  let as1 = assumptions th1 in
  let as2 = assumptions th2 in
  let c1 = consequence th1 in
  let c2 = consequence th2 in
  match c1 with
  | Imp (a, b) when a = c2 -> ImpE {x = th1; y = th2; a = (sum as1 as2); f = b}
  | _ -> raise (RulesErrors "error in implication elimination")

let bot_e f thm =
  match consequence thm with
  | Neg -> NegE {a = (assumptions thm); f = f; x = thm}
  | _ -> raise (RulesErrors "error in bottom elimination")

