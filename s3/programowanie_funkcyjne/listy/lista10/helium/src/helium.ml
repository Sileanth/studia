
let usage_string =
  "Usage: helium [OPTION]... [FILE [CMD_ARG]...]\nAvailable OPTIONs are:"

let args = ref []

let cmd_args_options = Arg.align (
  [ "-I",
    Arg.String Settings.add_source_dir,
    "<dir> Add <dir> to the list of include directories"
  ; "-no-stdlib",
    Arg.Unit (fun () -> Settings.set_lib_dirs []),
    " Do not use standard library"
  ; "-no-prelude",
    Arg.Unit (fun () -> Settings.set_prelude None),
    " Do not open Prelude module"
  ; "-prelude",
    Arg.String (fun s -> Settings.set_prelude (Some s)),
    "<module> Use <module> instead of Prelude"
  ; "-open",
    Arg.String Settings.add_auto_open,
    "<module> Open <module> before typing"
  ; "-where",
    Arg.Unit (fun () ->
      List.iter print_endline (Settings.lib_dirs ()); exit 0),
    " Print location of standard library and exit"
  ; "-simpl-schemes",
    Arg.Unit (fun () -> Settings.simpl_schemes := true),
    " Simplify type schemes (default)"
  ; "-no-simpl-schemes",
    Arg.Unit (fun () -> Settings.simpl_schemes := false),
    " Do not simplify type schemes"
  ; "-auto-lift",
    Arg.Unit (fun () -> Settings.auto_lift := Settings.ALP_Normal),
    " Try to infer lifting and swapping coercions (default)"
  ; "-no-auto-lift",
    Arg.Unit (fun () -> Settings.auto_lift := Settings.ALP_No),
    " Do not infer lifting and swapping coercions"
  ; "-pretty-rows",
    Arg.Unit (fun () -> Settings.pretty_rows := true),
      " Simplify effects before pretty-printing (default)"
  ; "-no-pretty-rows",
    Arg.Unit (fun () -> Settings.pretty_rows := false),
      " Do not simplify effects before pretty-printing"
  ; "-args",
    Arg.Rest (fun arg -> args := arg :: !args),
    "[CMD_ARG]... Pass remaining arguments to interpreted program"
  ] @ Flow.cmd_args_options ())

let fname = ref None
let proc_arg arg =
  match !fname with
  | None   ->
    fname := Some arg;
    (* Hack: change current argument to -args and reparse it in order
      to pass remaining arguments to interpreted program *)
    Sys.argv.(!Arg.current) <- "-args";
    Arg.current := !Arg.current - 1
  | Some _ -> assert false

let _ =
  Predef.StdExtern.register ();
  try
    Arg.parse cmd_args_options proc_arg usage_string;
    Settings.set_args (List.rev !args);
    match !fname with
    | None ->
      Settings.require_lib "REPL";
      Settings.set_repl
        { Settings.repl        = REPL.continue
        ; Settings.print_value = REPL.print_value
        };
      Flow.proc_stream CommonTags.eval stdin
    | Some fname ->
      Settings.set_current_file_path fname;
      Flow.proc_fname CommonTags.eval fname
  with
  | Flow.Flow_error err ->
    Box.print_stderr (Pretty.Errors.pretty_flow_error err);
    exit 1
  | Parser.Parse_error err ->
    Box.print_stderr (Pretty.Errors.pretty_parse_error err);
    exit 1
  | Transform.Raw.ScopeCheck.Scope_error err ->
    Box.print_stderr (Pretty.Errors.pretty_scope_error err);
    exit 1
  | Transform.Flat.TypeInference.Type_error err ->
    Box.print_stderr (Pretty.Errors.pretty_type_error err);
    exit 1
  | Transform.Unif.ToExplicit.Error err ->
    Box.print_stderr (Pretty.Errors.pretty_match_error err);
    exit 1
