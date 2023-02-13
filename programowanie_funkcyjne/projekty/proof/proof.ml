
open Formula

module type Theory = sig 
  type axiom 
  val axiom : axiom -> formula 
end

module Make(T : Theory) = struct
  module AxiomLogic = Logic.Make(T)
  open AxiomLogic
  open Formula
  
  module StringMap = Map.Make(String)
  
  type assump = (formula StringMap.t)
  type free_binding = (var StringMap.t *  string VarMap.t )
  
  type node = formula * assump * free_binding
  type goal = formula * assump * free_binding

  let get_free_binding (_, _, fb) : free_binding =
          fb
 
  module ProofTree : sig 
    type all_elim_info = term
    type ex_intro_info = var * term * formula
    type equiv_info = formula
    type ren_info = var * var

    type proof_tree 
    type view = 
      | VGoal of goal
      | VLemat of theorem
      | VAndI  of node * proof_tree * proof_tree
      | VAndE1 of node * proof_tree
      | VAndE2 of node * proof_tree
      | VImpE  of node * proof_tree * proof_tree
      | VImpI  of node * proof_tree
      | VBotE  of node * proof_tree
      | VAllI  of node * proof_tree
      | VAllE  of node * proof_tree * all_elim_info 
      | VExI   of node * proof_tree * ex_intro_info 
      | VExE   of node * proof_tree * proof_tree
      | VOrI1  of node * proof_tree
      | VOrI2  of node * proof_tree
      | VOrE   of node * proof_tree * proof_tree * proof_tree
      | VEquiv of node * proof_tree * equiv_info 
      | VRen   of node * proof_tree * ren_info

 
    val view : proof_tree -> view  

    val goal : goal -> proof_tree
    val lemat : theorem -> proof_tree
    val andI : node -> proof_tree -> proof_tree -> proof_tree
    val andE1 : node -> proof_tree -> proof_tree
    val andE2 : node -> proof_tree -> proof_tree
    val impE : node -> proof_tree -> proof_tree -> proof_tree
    val impI : node -> proof_tree -> proof_tree
    val botE : node -> proof_tree -> proof_tree
    val allI : node -> proof_tree -> proof_tree
    val allE : all_elim_info -> node -> proof_tree -> proof_tree
    val exI : ex_intro_info -> node -> proof_tree -> proof_tree
    val exE : node -> proof_tree -> proof_tree -> proof_tree
    val orI1 : node -> proof_tree -> proof_tree
    val orI2 : node -> proof_tree -> proof_tree
    val orE : node -> proof_tree -> proof_tree -> proof_tree -> proof_tree
    val equiv_b : node -> equiv_info -> proof_tree -> proof_tree
    val ren_b : node -> ren_info -> proof_tree -> proof_tree
  end = struct  
    
    type all_elim_info = term
    type ex_intro_info = var * term * formula
    type equiv_info = formula
    type ren_info = var * var

    type proof_tree =
      | Goal  of goal
      | Lemat of theorem
      | AndI  of node * proof_tree * proof_tree
      | AndE1 of node * proof_tree
      | AndE2 of node * proof_tree
      | ImpE  of node * proof_tree * proof_tree
      | ImpI  of node * proof_tree
      | BotE  of node * proof_tree
      | AllI  of node * proof_tree
      | AllE  of node * proof_tree * all_elim_info 
      | ExI   of node * proof_tree * ex_intro_info 
      | ExE   of node * proof_tree * proof_tree
      | OrI1  of node * proof_tree
      | OrI2  of node * proof_tree
      | OrE   of node * proof_tree * proof_tree * proof_tree
      | Equiv of node * proof_tree * equiv_info 
      | Ren   of node * proof_tree * ren_info
    
    type view = 
      | VGoal  of goal
      | VLemat of theorem
      | VAndI  of node * proof_tree * proof_tree
      | VAndE1 of node * proof_tree
      | VAndE2 of node * proof_tree
      | VImpE  of node * proof_tree * proof_tree
      | VImpI  of node * proof_tree
      | VBotE  of node * proof_tree
      | VAllI  of node * proof_tree
      | VAllE  of node * proof_tree * all_elim_info 
      | VExI   of node * proof_tree * ex_intro_info 
      | VExE   of node * proof_tree * proof_tree
      | VOrI1  of node * proof_tree
      | VOrI2  of node * proof_tree
      | VOrE   of node * proof_tree * proof_tree * proof_tree
      | VEquiv of node * proof_tree * equiv_info 
      | VRen   of node * proof_tree * ren_info


    let view = function 
      | Goal (g)                -> VGoal (g)
      | Lemat (th)              -> VLemat (th)
      | AndI (n, pf1, pf2)      -> VAndI (n, pf1, pf2)
      | AndE1 (n, pf)           -> VAndE1 (n, pf)  
      | AndE2 (n, pf)           -> VAndE2 (n, pf)
      | ImpE (n, pf1, pf2)      -> VImpE (n, pf1, pf2)
      | ImpI (n,pf)             -> VImpI (n,pf)
      | BotE (n, pf)            -> VBotE (n, pf)
      | AllI (n, pf)            -> VAllI (n, pf) 
      | AllE (n,pf,info)        -> VAllE (n,pf,info)
      | ExI (n, pf, info)       -> VExI (n, pf, info)
      | ExE (n, pf1, pf2)       -> VExE (n, pf1, pf2)
      | OrI1 (n, pf)            -> VOrI1 (n, pf)
      | OrI2 (n,pf)             -> VOrI2 (n,pf)
      | OrE  (n, pf1, pf2, pf3) -> VOrE  (n, pf1, pf2, pf3)
      | Equiv (n, pf, info)     -> VEquiv (n, pf, info)
      | Ren (n, pf, info)       -> VRen (n, pf, info)



    (* Smart constructors*)
    let goal g = Goal g
    
    let lemat th = Lemat th
    

    let form_node (f, _, _) = f

    let ren_b node (v1, v2) = function 
      | Lemat th -> Lemat(ren th v1 v2)
      | pf       -> Ren (node, pf, (v1, v2))

    let equiv_b node f = function 
      | Lemat th -> Lemat (equiv th f)
      | pf       -> Equiv (node, pf, f)

    (* Implication smart constructors *)
    
    let impE node pf1 pf2 = match pf1, pf2 with
      | Lemat th1, Lemat th2 -> Lemat(imp_e th1 th2) 
      | pf1, pf2             -> ImpE (node, pf1, pf2)

    let impI node = function 
      | Lemat th -> Lemat (imp_i (form_node node) th)
      | pf       -> ImpI(node, pf) 
    
    let botE node = function 
      | Lemat th -> Lemat (bot_e th  (form_node node))
      | pf       -> BotE (node, pf)
    
    let allI node = function 
      | Lemat th -> Lemat (all_i th)
      | pf       -> AllI (node, pf)

    let allE term node = function 
      | Lemat th -> Lemat (all_e th term)
      | pf       -> AllE (node, pf, term)

    let exI (v, t, f) node = function 
      | Lemat th -> Lemat (ex_i th v t f)
      | pf       -> ExI (node, pf, (v, t, f))

    let exE node pf1 pf2 = match pf1, pf2 with
    | Lemat th1, Lemat th2 -> Lemat (ex_e th1 th2)
    | pf1, pf2             -> ExE (node, pf1 ,pf2)

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

  end
  open ProofTree
  type context = 
    | CRoot 
    | CAndIL  of context * node * proof_tree
    | CAndIR  of context * node * proof_tree
    | CAndE1  of context * node
    | CAndE2  of context * node
    | CImpeEL of context * node * proof_tree  
    | CImpeER of context * node * proof_tree  
    | CimpI   of context * node
    | CBotE   of context * node 
    | CAllI   of context * node
    | CAllE   of context * node * all_elim_info
    | CExI    of context * node * ex_intro_info 
    | CExEL   of context * node * proof_tree
    | CExER   of context * node * proof_tree
    | COrI1   of context * node
    | COrI2   of context * node
    | COrEL   of context * node * proof_tree * proof_tree
    | COrER   of context * node * proof_tree * proof_tree
    | COrEM   of context * node * proof_tree * proof_tree
    | CEquiv  of context * node * equiv_info 
    | CRen    of context * node * ren_info

  type proof =
    | Complete of theorem * free_binding
    | Sketch of goal * context

  let qed = function
  | Complete (th, fb) -> th, fb
  | Sketch _    -> failwith "proof is incomplete"

  let proof form assum bind =
    Sketch ((form, assum, bind), CRoot)


  let string_theorem bind (th : theorem) =
          let f = AxiomLogic.consequnce th in
          let assu = AxiomLogic.assumptions th in
          let f_string = string_of_formula bind f in
          let assu_string = 
                  List.fold_right (fun ass acc -> acc ^ " | " ^ string_of_formula bind ass) assu "" in
          assu_string ^ " ⊢ " ^ f_string

  let proof_string (p : proof) =
          match p with
          | Complete (th, (_, bind)) -> "Complete Proof: " ^ string_theorem bind th
          | Sketch ((f, assump, (_, bind)), ctx) ->
                          let assump_list = assump |> StringMap.to_seq |> List.of_seq in
                          let assump_string = List.fold_right 
                                (fun (name, f) acc -> 
                                        acc ^ " | " ^ name ^ " : " ^ string_of_formula bind f)
                                assump_list ""
                          in "Sketch Proof: " ^ assump_string ^  " ⊢ " ^ string_of_formula bind f
                

  let up_proof_tree (pf : proof_tree) (ctx : context) : (proof_tree * context) = 
      match ctx with
        | CRoot -> failwith "you are on top of proof tree"
        | CAndIL (ctx, n, r)   -> andI n pf r, ctx
        | CAndIR (ctx, n, l)   -> andI n l pf, ctx
        | CAndE1 (ctx, n)      -> andE1 n pf, ctx 
        | CAndE2 (ctx, n)      -> andE2 n pf, ctx
        | CImpeEL(ctx, n, r)   -> impE n pf r, ctx 
        | CImpeER (ctx, n,  l) -> impE n l pf, ctx   
        | CimpI (ctx, n)       -> impI n pf, ctx 
        | CBotE (ctx, n)       -> botE n pf, ctx 
        | CAllI (ctx, n)       -> allI n pf, ctx 
        | CAllE (ctx, n, i)    -> allE i n pf, ctx 
        | CExI (ctx, n, i)     -> exI i n pf, ctx
        | CExEL (ctx, n, r)    -> exE n pf r, ctx
        | CExER (ctx, n, l)    -> exE n l pf, ctx
        | COrI1 (ctx, n)       -> orI1 n pf, ctx
        | COrI2 (ctx, n)       -> orI2 n pf, ctx 
        | COrEL (ctx, n, m, r) -> orE n pf m r, ctx
        | COrEM (ctx, n, l, r) -> orE n l pf r, ctx
        | COrER (ctx, n, l, m) -> orE n l m pf, ctx
        | CEquiv (ctx, n, i)   -> equiv_b n i pf, ctx
        | CRen (ctx, n, i)     -> ren_b n i pf, ctx
  (* ignores lemat nodes *)
  let down_left_tree (pf : proof_tree) (ctx : context) : (proof_tree * context) =
    match view pf with
      | VGoal (g)                -> pf, ctx
      | VLemat (th)              -> pf, ctx 
      | VAndI (n, pf1, pf2)      -> 
        begin match view pf1 with
          | VLemat _ -> pf2, CAndIR (ctx, n, pf1)
          | _         -> pf1, CAndIL (ctx, n, pf2)
        end
      | VAndE1 (n, pf)           -> pf, CAndE1 (ctx, n)
      | VAndE2 (n, pf)           -> pf, CAndE2 (ctx, n)
      | VImpE (n, pf1, pf2)      -> 
          begin match view pf1 with 
            | VLemat _ -> pf2, CImpeER (ctx, n, pf1)
            | _        -> pf1, CImpeEL (ctx, n, pf2)
          end
      | VImpI (n,pf)             -> pf, CimpI (ctx, n)
      | VBotE (n, pf)            -> pf, CBotE (ctx, n) 
      | VAllI (n, pf)            -> pf, CAllI (ctx, n)
      | VAllE (n,pf,info)        -> pf, CAllE (ctx, n, info)
      | VExI (n, pf, info)       -> pf, CExI (ctx, n, info)
      | VExE (n, pf1, pf2)       -> 
          begin match view pf1 with 
            | VLemat _ -> pf2, CExER (ctx, n, pf1)
            | _        -> pf1, CExEL(ctx, n, pf2)
          end
      | VOrI1 (n, pf)            -> pf, COrI1 (ctx, n)
      | VOrI2 (n,pf)             -> pf, COrI2 (ctx, n)
      | VOrE  (n, pf1, pf2, pf3) -> 
          begin match view pf1, view  pf2 with
            | VLemat _, VLemat _ -> pf3, COrER (ctx, n, pf1, pf2)
            | VLemat _, _        -> pf2, COrEL (ctx, n, pf1, pf3)
            | _, _               -> pf1, COrEL (ctx, n, pf2, pf3)
          end
      | VEquiv (n, pf, info)     -> pf, CEquiv (ctx, n, info)
      | VRen (n, pf, info)       -> pf, CRen (ctx, n, info)

  let rec to_the_left_bottom (pf : proof_tree) (ctx : context) : proof_tree * context =
    match view pf with
      | VGoal (g) -> goal g, ctx
      | VLemat th -> lemat th, ctx
      | _         -> let npf, nctx = down_left_tree pf ctx in to_the_left_bottom npf nctx

  let rec rek (pf : proof_tree) (ctx : context) : (proof_tree * context) =
    let up () = let npf, nctx = up_proof_tree pf ctx in 
      rek npf nctx
    in match ctx with 
      | CRoot  -> to_the_left_bottom pf ctx 
      | CAndIL (ctx, n, r) -> 
          to_the_left_bottom r (CAndIL (ctx, n, pf)) 
      | CImpeEL (ctx, n, r) ->
        begin match view r with 
          | VLemat _ -> up ()
          | _        -> to_the_left_bottom r (CImpeER (ctx, n, pf))  
        end
      | CExEL (ctx, n, r) ->
        begin match view r with 
          | VLemat _ -> up ()
          | _        ->  to_the_left_bottom r (CExER (ctx, n, pf)) 
        end
      | COrEL (ctx, n, m, r) -> 
        begin match view m, view r with 
          | VLemat _, VLemat _ -> up () 
          | VLemat _, _        -> to_the_left_bottom r (COrER (ctx, n, pf, m))  
          | _, _               -> to_the_left_bottom m (COrEM (ctx, n, pf, r))  
        end
      | COrEM (ctx, n, l, r) -> 
        begin match view r with 
          | VLemat _ -> up () 
          | _        -> to_the_left_bottom r (COrER (ctx, n, l, pf))  
        end
      | _ -> up ()

  let next_goal (p : proof) : proof =
    match p with
    | Complete _     -> failwith "dowód jest skończony"
    | Sketch (g, ctx) -> 
        let pf, ctx  = rek (goal g) ctx in 
        begin match view pf with 
          | VGoal g   -> Sketch (g, ctx)
          | VLemat th -> Complete (th, get_free_binding g) 
          | _       -> failwith "nieoczekiwany błąd"
        end

  let proof_tree_to_proof pf ctx =
          match view pf with
          | VGoal g   -> Sketch (g, ctx)
          | VLemat th -> Complete (th, (StringMap.empty, VarMap.empty))
          | _         -> failwith "ojoj"


  let intro name = function 
    | Complete _ -> failwith "dowód jest skończony"
    | Sketch ((f, a, (sb, bs)), ctx) -> match f with
      | Imp (l,r) -> 
        begin if StringMap.exists (fun s _ -> s = name ) a  
          then failwith "nazwa jest zajęta"
          else 
            let (ng : goal) = r, (StringMap.add name l a), (sb, bs) 
            in let parent = CimpI (ctx, (l, a, (sb, bs))) 
            in Sketch (ng,parent) 
        end
      | _ -> failwith "nie jest to implikacja"

  let apply_assumption name = function
    | Complete _ -> failwith "dowód jest skończony"
    | Sketch ((f, assump, (sb,bs)), ctx) -> let a = StringMap.find name assump in
        assert (f = a);
        let pft = ProofTree.lemat (AxiomLogic.by_assumption f) in
        let npft, nctx = rek pft ctx in
        (proof_tree_to_proof npft nctx)

  let apply_theorem (th : theorem) = function
          | Complete _ -> failwith "dowód jest skończony"
          | Sketch ((f, assump, bind), ctx) ->
                          let _ = AxiomLogic.consequnce th in
                          let _ = AxiomLogic.assumptions th in
                          let pft = ProofTree.lemat th in
                          let npft, nctx = rek pft ctx in
                          (proof_tree_to_proof npft nctx)

  let rec imp_chain goal = function
          | f when eq_formula f goal -> []
          | Imp (a, b)      -> List.append (imp_chain goal b) [a]
          | _               -> failwith "not implication and not end goal"

  let rec imp_chain_proof_tree parent_ctx imp assumption bind (xs : formula list) =
          match xs with
          | []  -> failwith "pusty łańcuch implikacji"
          | [x] -> 
                let goal = (Imp (x, imp), assumption, bind) in
                let left_goal = ProofTree.goal (x, assumption, bind) in
                let ctx = CImpeEL (parent_ctx, (imp, assumption, bind), left_goal) in
                Sketch (goal, ctx)
          | x :: xs -> 
                let left_goal = ProofTree.goal (x, assumption, bind) in
                let ctx = CImpeEL (parent_ctx, (imp, assumption, bind), left_goal) in
                imp_chain_proof_tree ctx (Imp (x, imp)) assumption bind xs

                
  let apply_implication imp = function
    | Complete _ -> failwith "dowód jest skończony"
    | Sketch ((f, assump, (sb,bs)), ctx) ->
        let (imps : formula list) = imp_chain f imp in
        imp_chain_proof_tree ctx f assump (sb,bs) imps

  let exfalso = function
    | Complete _ -> failwith "dowód jest skończony"
    | Sketch ((f, assump, bind), ctx) ->
                    let goal = (Neg, assump, bind) in
                    let nctx = CBotE (ctx, (f, assump, bind)) in
                    Sketch (goal, nctx)


  let and_construct = function 
    | Complete _ -> failwith "dowód jest skończony" 
    | Sketch ((f, a, (sb, bs)), ctx) -> match f with 
      | And (l, r) ->
        let ng = l, a, (sb, bs) in 
        let r = goal (l, a, (sb, bs)) in 
        let parent = CAndIL (ctx, ((f, a, (sb, bs))), r) in 
        Sketch (ng, parent)
      | _ -> failwith "to nie jest koniunkcja"
  
  let and_elim_1 r = function 
    | Complete _ -> failwith "dowód jest nieskończony"
    | Sketch ((f, a, fb), ctx) -> 
        let ng = And (f, r), a, fb in
        let parent = CAndE1 (ctx, (f,a, fb)) in 
        Sketch (ng, parent)
   
   let and_elim_2 l = function 
    | Complete _ -> failwith "dowód jest nieskończony"
    | Sketch ((f, a, fb), ctx) -> 
        let ng = And (f, l), a, fb in
        let parent = CAndE2 (ctx, (f,a, fb)) in 
        Sketch (ng, parent)
  
    let or_construct_1 = function 
      | Complete _ -> failwith "dowód jest skończony" 
      | Sketch ((f, a, (sb, bs)), ctx) -> match f with 
        | Or (l, r) ->
          let ng = l, a, (sb, bs) in 
          let parent = COrI1 (ctx, ((f, a, (sb, bs)))) in 
          Sketch (ng, parent)
        | _ -> failwith "to nie jest koniunkcja"
    
    let or_construct_2 = function 
      | Complete _ -> failwith "dowód jest skończony" 
      | Sketch ((f, a, (sb, bs)), ctx) -> match f with 
        | Or (l, r) ->
          let ng = r, a, (sb, bs) in 
          let parent = COrI2 (ctx, ((f, a, (sb, bs)))) in 
          Sketch (ng, parent)
        | _ -> failwith "to nie jest koniunkcja"

    let forall_intro = function 
      | Complete _ -> failwith "dowód jest skończony"
      | Sketch ((f, a, fb), ctx) -> match f with 
        | All (name, nf) -> 
          let ng = nf, a, fb in 
          let parent = CAllI (ctx, (f, a, fb)) in 
          Sketch (ng, parent)
        | _ -> failwith "f <> forall"

    let forall_exlim form name (t : term) = function 
      | Complete _ -> failwith "dowód nie jest skończony"
      | Sketch ((f, a, (x, fb)), ctx) ->
          let all = All (name, form) in
          if eq_formula f (subst_in_formula 0 t form) 
          then
            let nfb = VarMap.add 0 name fb in
            let ng = all, a, (x, nfb) in  
            let parent = CAllE (ctx, (f, a, (x, fb)), t)
            in Sketch (ng, parent) 
          else
           ( 
                   (*
           (print_endline (string_of_formula VarMap.empty form)); 
           (Sketch ((f, a, (x, fb)), ctx)) *)
           failwith "niepoprawna formuła i term"
           )
    
    let rename_kwant name = function 
      | Complete _ -> failwith "dowód jest skończony" 
      | Sketch ((f, a, fb), ctx) -> match f with 
        | All (old_name, af) ->
            let new_f = All (name, af)
            in Sketch ((new_f, a, fb), CEquiv (ctx, (f, a, fb), f))
        | Ex (old_name, af) ->
          let new_f = Ex (name, af)
          in Sketch ((new_f, a, fb), CEquiv (ctx, (f, a, fb), f))
        | _ -> failwith "f <> kwant" 




 end


        


   
