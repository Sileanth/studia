(** The evaluator *)

open Ast


type value = 
| VUnit
| VNum of int 
| VLambda of var * expr


module M = Map.Make(String)
type env = value M.t

let eval (env : env) ( expr : expr) :  value option =
  match expr.data with 
  | EUnit -> Some VUnit 
  | ENum n -> Some (VNum n) 
  | EVar vr ->  M.find_opt vr env
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
