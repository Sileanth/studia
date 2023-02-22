open Lang.Node
module S = Lang.Raw
module T = Lang.Flat

let tr_repl_cmd repl env cmd =
  let make data = { meta = cmd.meta; data = data } in
  match cmd.data with
  | S.ReplExit ->
    print_newline ();
    exit 0
  | S.ReplExpr e ->
    let e = Expr.tr_expr env e in
    make (T.EReplExpr(e, fun () -> repl env))
  | S.ReplDef def ->
    let (env, defs) = Expr.tr_defs env [ def ] in
    Context.plug defs (make (T.ERepl(fun () -> repl env)))
