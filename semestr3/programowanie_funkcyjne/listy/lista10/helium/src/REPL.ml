
let print_value seal v =
  let tp  = Utils.Seal.find Lang.Unif.Keys.repl_expr_type seal in
  let env = Pretty.Unif.create_tvar_env () in
  print_endline v;
  Box.print_stdout
    (Box.prefix (Box.word ": ") (Pretty.Unif.pretty_type env 0 tp))

let print_prompt seal =
  let eff = Utils.Seal.find Lang.Unif.Keys.repl_effect seal in
  let env = Pretty.Unif.create_tvar_env () in
  let box =
    Box.suffix (Pretty.Unif.pretty_arrow_effect env eff) (Box.word "> ") in
  Box.print_stdout_no_nl box;
  flush stdout

let rec continue seal repl =
  print_prompt seal;
  try repl () with
  | Flow.Flow_error err ->
    Box.print_stderr (Pretty.Errors.pretty_flow_error err);
    flush stderr;
    continue seal repl
  | Parser.Parse_error err ->
    Box.print_stderr (Pretty.Errors.pretty_parse_error err);
    flush stderr;
    continue seal repl
  | Transform.Raw.ScopeCheck.Scope_error err ->
    Box.print_stderr (Pretty.Errors.pretty_scope_error err);
    flush stderr;
    continue seal repl
  | Transform.Flat.TypeInference.Type_error err ->
    Box.print_stderr (Pretty.Errors.pretty_type_error err);
    flush stderr;
    continue seal repl
  | Transform.Unif.ToExplicit.Error err ->
    Box.print_stderr (Pretty.Errors.pretty_match_error err);
    flush stderr;
    continue seal repl
