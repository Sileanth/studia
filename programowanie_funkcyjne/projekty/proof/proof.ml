
open Formula

module type Theory = sig 
  type axiom 
  val axiom : axiom -> formula 
end

module Make(T : Theory) = struct
  module AxiomLogic = Logic.Make(T)
  open AxiomLogic
  
  type assump = (string * theorem) list
  type free_binding = (var * string) list
  
  type goal = formula * assump * free_binding

  type proof_tree =
    | Goal of goal
    | Lemat of theorem
    | Rule of proof_tree * formula

  type context = 
    | Root
    | Rule of formula

  type proof =
    | Complete of theorem
    | Sketch of goal * context


end
