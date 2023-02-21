open Logic

type target = (string * formula) list * formula

type proof_tree =
  | Target   of target
  | Finished of theorem
  | ImplI    of proof_tree * formula
  | ImplE    of proof_tree * proof_tree
  | BotE     of proof_tree * formula


let smart_botE proof_tree formula = 
  match proof_tree with
    | Finished theorem -> Finished ( Logic.bot_e formula theorem )
    | _ -> BotE proof_tree formula

(* smart konstruktory *)

type proof_context = 
  | Root
  | ImplI       of proof_context * formula
  | ImplE_Left  of proof_context * proof_tree
  | ImplE_Right of proof_tree * proof_context
  | BotE        of proof_context * formula

type proof =
  | Finished     of proof_tree
  | ActiveTarget of target * proof_context

let proof g f = ActiveTarget((g, f), Root)

let qed =
  let rec build_proof = function
    | Target _      -> failwith "Cannot build proof with unfinished target"
    | Finished thm  -> thm
    | ImplI(p, f)   -> imp_i f (build_proof p)
    | ImplE(p1, p2) -> imp_e (build_proof p1) (build_proof p2)
    | BotE(p, f)    -> bot_e f (build_proof p)
  in function
    | Finished p         -> build_proof p
    | ActiveTarget(_, _) -> failwith "Cannot qed on ActiveTarget"

let goal = function
  | Finished _                 -> None
  | ActiveTarget((assm, f), _) -> Some(assm, f)

let rec find_target_down tree ctx = match tree with
  | Target tg     -> Some(ActiveTarget(tg, ctx))
  | Finished _    -> None
  | ImplI(p, f)   -> find_target_down p (ImplI(ctx, f))
  | ImplE(p1, p2) ->
    begin match find_target_down p1 (ImplE_Left(ctx, p2)) with
      | Some p -> Some p
      | None   -> find_target_down p2 (ImplE_Right(p1, ctx))
    end
  | BotE(p, f)    -> find_target_down p (BotE(ctx, f))
  
let rec find_target tree ctx = match ctx with
  | Root                  -> find_target_down tree ctx
  | ImplI(ctx2, f)        -> find_target (ImplI(tree, f)) ctx2
  | ImplE_Left(ctx2, pt)  ->
    begin match find_target_down pt (ImplE_Right(tree, ctx2)) with
      | Some p -> Some p
      | None   -> find_target (ImplE(tree, pt)) ctx2
    end
  | ImplE_Right(pt, ctx2) -> find_target (ImplE(pt, tree)) ctx2
  | BotE(ctx2, f)         -> find_target (BotE(tree, f)) ctx2

let next = function
  | Finished _            -> failwith "Cannot find next target in finished proof"
  | ActiveTarget(tg, ctx) -> find_target (Target tg) ctx |> Option.get

let intro name pf = match pf with
  | Finished _ ->
    failwith "Cannot introduce implication - finished proof"
  | ActiveTarget((assm, Impl(psi, phi)), ctx) ->
    ActiveTarget(((name, psi) :: assm, phi), ImplI(ctx, psi))
  | ActiveTarget(_, _) -> 
    failwith "Cannot introduce implication - formula is not an implication"

let uni_apply (f: formula) (pf : proof) (fill : proof_tree) : proof =
  let rec matches_target f tf =
    match compare f tf, f with
      | 0, _          -> true
      | _, Impl(_, f) -> matches_target f tf
      | _, _          -> false
  in let rec build_subtree assm tree f tf =
    if compare f tf = 0 then tree
    else match f with
      | Impl(a, b) ->
        let tree = ImplE(tree, Target(assm, a)) in
        build_subtree assm tree b tf
      | _          -> failwith "Dead code"
  in let rec build_tree (tree : proof_tree) ctx =
    match ctx with
      | Root                 -> tree
      | ImplI(ctx, f)        -> build_tree (ImplI(tree, f)) ctx
      | ImplE_Left(ctx, pt)  -> build_tree (ImplE(tree, pt)) ctx
      | ImplE_Right(pt, ctx) -> build_tree (ImplE(pt, tree)) ctx
      | BotE(ctx, f)         -> build_tree (BotE(tree, f)) ctx
  in match pf with
    | Finished _                    -> failwith "Cannot apply - finished proof"
    | ActiveTarget((assm, tf), ctx) ->
      let tree =
        if matches_target f tf
        then build_subtree assm fill f tf
        else if matches_target f Bot
        then BotE(build_subtree assm fill f Bot, tf)
        else failwith "Cannot apply - provided formula does not match target"
      in match find_target_down tree ctx with
        | Some p -> p
        | None   ->
          begin match find_target tree ctx with
            | Some p -> p
            | None   -> Finished (build_tree tree ctx)
          end

let apply f pf = match pf with
  | Finished _                 -> failwith "Cannot apply - finished proof"
  | ActiveTarget((assm, _), _) -> uni_apply f pf (Target(assm, f))

let apply_thm thm pf = uni_apply (consequence thm) pf (Finished thm)

let apply_assm name pf = match pf with
  | Finished _                 -> failwith "Cannot apply - finished proof"
  | ActiveTarget((assm, _), _) ->
    let f = (List.assoc name assm) in
    uni_apply f pf (Finished (by_assumption f))

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()