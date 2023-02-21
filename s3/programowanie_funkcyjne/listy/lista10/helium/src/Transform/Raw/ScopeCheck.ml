open ScopeCheckPriv
open Lang.Node
module S = Lang.Raw
module T = Lang.Flat

include Errors

let tr_defs env ?intf fname =
  let mpath = Env.current_path env in
  match intf with
  | None ->
    let file = Parser.parse_file fname in
    let (defs1, str) = Expr.tr_struct env ~mpos:file.meta file.data in
    let (defs2, ns)  = ModuleSystem.to_namespace ~mpath str in
    (ns, defs1 @ defs2)
  | Some intf ->
    let intf = Parser.parse_intf_file intf in
    let intf = ModuleType.tr_struct_sig env ~mpos:intf.meta intf.data in
    let file = Parser.parse_file fname in
    let mexp = Expr.tr_struct env ~mpos:file.meta file.data in
    let str  = ModuleSystem.match_signature mexp intf in
    let (defs, ns)  = ModuleSystem.to_namespace ~mpath str in
    (ns, defs)

let init_env () =
  let env = Env.init tr_defs in
  List.fold_left Env.auto_open env (Settings.auto_open ())


let tr_program file =
  let meta = Utils.Position.nowhere in
  let env = init_env () in
  let (ctx, _) = Expr.tr_struct env ~mpos:file.meta file.data in
  let ctxs = Env.req_modules_rev env in
  List.fold_left (fun e ctx -> Context.plug ctx e)
    { meta; data = T.ENum 0 }
    (ctx :: ctxs)

let rec repl chan env =
  let cmd = Parser.parse_repl_cmd chan in
  Env.clean_req_modules env;
  let body = Repl.tr_repl_cmd (repl chan) env cmd in
  let ctxs = Env.req_modules_rev env in
  List.fold_left (fun e ctx -> Context.plug ctx e) body ctxs

let init_repl chan =
  let meta = Utils.Position.nowhere in
  let env  = init_env () in
  let ctxs = Env.req_modules_rev env in
  List.fold_left (fun e ctx -> Context.plug ctx e)
    { meta; data = T.ERepl (fun () -> repl chan env) }
    ctxs

let _ =
  Flow.register_transform
    ~source: S.flow_node
    ~target: T.flow_node
    tr_program;
  Flow.register_repl
    ~target: T.flow_node
    init_repl
