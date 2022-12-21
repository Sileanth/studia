
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
  
  type node = formula * assump * free_binding
  type goal = formula * assump * free_binding

  type proof_tree =
    | Goal of goal
    | Lemat of theorem
    | AndI  of node * proof_tree * proof_tree
    | AndE1 of node * proof_tree
    | AndE2 of node * proof_tree
    | ImpE  of node * proof_tree * proof_tree
    | ImpI  of node * proof_tree
    | BotE  of node * proof_tree
    | AllI  of node * proof_tree
    | AllE  of node * proof_tree
    | ExI   of node * proof_tree
    | ExE   of node * proof_tree * proof_tree
    | OrI1  of node * proof_tree
    | OrI2  of node * proof_tree
    | OrE   of node * proof_tree * proof_tree * proof_tree
    | Equiv of node * proof_tree
    | Ren   of node * proof_tree

  (* Smart constructors*)
  let goal g = Goal g
  
  let lemat th = Lemat th
  
  let andI node pf1 pf2 = match pf1, pf2 with
  | Lemat th1, Lemat th2 -> Lemat (and_i th1 th2)
  | pf1, pf2             -> AndI (node, pf1, pf2)
  
  let andE1 node = function
  | Lemat th -> Lemat (and_e1 th)
  | pf       -> AndE1 (node, pf)

  let andE2 node = function
  | Lemat th -> Lemat (and_e2 th)
  | pf       -> AndE2 (node, pf)

  let orI1 node = function
  | Lemat th -> begin match node with 
    | (Or (a,b ), _, _) -> Lemat (or_i1 th b)
    | _                 -> failwith "error" 
    end
  | pf       -> OrI1 (node, pf)

  let orI2 node = function
  | Lemat th -> begin match node with 
    | (Or (a,b ), _, _) -> Lemat (or_i2 th a)
    | _                 -> failwith "error" 
    end
  | pf       -> OrI1 (node, pf)

  let orE node pf1 pf2 pf3 = 
    match pf1, pf2, pf3 with
    | Lemat th1, Lemat th2, Lemat th3 -> Lemat (or_e th1 th2 th3)
    | pf1, pf2, pf3                   -> OrE (node, pf1, pf2, pf3)


  type context = 
    | Root
    | Rule of formula

  type proof =
    | Complete of theorem
    | Sketch of goal * context

  let qed = function
  | Complete th -> th
  | Sketch _    -> failwith "proof is incomplete"

  let proof form assum bind =
    Sketch ((form, assum, bind), Root)


end
