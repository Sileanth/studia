(** The evaluator *)





open Ast


type value = 
| VUnit
| VBool of bool
| VNum of int 
| VLambda of var * expr
| VFix of var * var * expr

type error_t

type 'a 'b res = 
| Correct of 'a  
| Error of 'b

module M = Map.Make(String)
type env = expr M.t

module EvalMonad : sig 
  type 'a 'b t

  val fail : 'b' -> 'a 'b t 
  val get : value 'b t 
  val set : var -> value -> unit 'b t 


end = struct
  
  type state = env 
  type 'a 'b t = state -> ('a, state) res

  let fail err state = Error err
  let get st -> Correct (st, st)


  let return a = fun state -> Correct (a, state)

  let set var valu state = 
    let new_state = M.add var valu state in 
    ((), new_state)


  let bind (x : 'x 'e t) (fa : 'x -> 'y 'e t) : 'y 'e t = 
     fun st -> 
      match x state with 
      | Error e -> Error e 
      | Correct x ->  fa x state  



    

  
 

end

let rec eval (env : env) ( expr : expr) :  value option =
  match expr.data with 
  | EUnit -> Some VUnit 
  | EBool b -> Some (VBool b)
  | ENum n -> Some (VNum n) 
  | EVar vr ->  M.find_opt vr env
  | EFn (vr, e) -> Some (VLambda (vr, e))
  | Efix (vr1, vr2, e) -> Some (VFix (vr1, vr2, e))
  | ESeq (e1, e1) -> match eval env e1 with 
    | 
  | _ -> failwith "not_implemnted"


let eval_program (p : Ast.program) =
  (* TODO: implement this function. Probably you will need to define a data
   * type for value representation, and write an auxiliary "eval" function
   * that takes an environment and an expression. *)
  match p.data with
  | ENum n ->
    (* Considering numbers here, separately from other constructs is probably
     * bad idea. This function should call eval function, but I've placed this
     * dummy pattern-matching here just to show how we can pattern-match on
     * expressions. *)
    Utils.report_error p "some runtime error related to value %d" n
  | _ ->
    Utils.report_error p
      "cannot evaluate this expression. The evaluator is not implemented yet!"
