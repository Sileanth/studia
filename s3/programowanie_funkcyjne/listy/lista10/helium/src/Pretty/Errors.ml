
let error msg =
  Box.tbox (Box.word "error:" :: Box.text_indent 2 :: msg)

let error_p pos msg =
  Box.tbox
    (  Box.word (Utils.Position.to_string pos ^ ":")
    :: Box.ws (Box.word "error:")
    :: Box.text_indent 2
    :: msg)

let pretty_flow_error err =
  match err with
  | Flow.PathNotFound tag ->
    error (Box.textfl "Cannot find path to %s." (Flow.Tag.name tag))
  | Flow.UnsupportedExtension ext ->
    error (Box.textfl "Unsupported file extension `%s'." ext)

let pretty_parse_error err =
  match err with
  | Parser.CannotOpenFile(path, msg) ->
    error
      ( Box.textl "Cannot open file"
      @ [ Box.ws (Box.word path) ]
      @ Box.textfl "(%s)." msg)
  | Parser.CannotReadFile(path, msg) ->
    error
      ( Box.textl "Cannot read file"
      @ [ Box.ws (Box.word path) ]
      @ Box.textfl "(%s)." msg)
  | Parser.UnexpectedChar(pos, c) ->
    error_p pos
      ( Box.textfl "Unexpected character"
      @ [ Box.ws (Box.word (Printf.sprintf "'%s'" (Char.escaped c))) ]
      @ Box.textfl "(0x%02X)." (Char.code c))
  | Parser.EofInComment pos ->
    error_p pos
      ( Box.textl "End of file inside block comment (missing `*)').")
  | Parser.EofInChar pos ->
    error_p pos
      ( Box.textl "End of file inside character literal (missing `'').")
  | Parser.EofInString pos ->
    error_p pos
      ( Box.textl "End of file inside string literal (missing `\"').")
  | Parser.InvalidNumber(pos, tok) ->
    error_p pos
      ( Box.textfl "Invalid numerical literal `%s'." tok)
  | Parser.InvalidEscape(pos, tok) ->
    error_p pos
      ( Box.textfl "Invalid character escape `%s'." tok)
  | Parser.InvalidString(pos, tok, err) ->
    error_p pos
      ( Box.textfl "Invalid string literal `%s': %s" tok err)
  | Parser.UnexpectedToken(pos, tok) ->
    error_p pos
      ( Box.textl "Syntax error. Unexpected token"
      @ [ Box.ws (Box.word (Printf.sprintf "`%s'." tok)) ])

let pretty_scope_error err =
  let open Transform.Raw.ScopeCheck in
  let name_of = function
    | N_Var    -> "variable"
    | N_Op     -> "operation"
    | N_Ctor   -> "constructor"
    | N_Field  -> "record field"
    | N_Type   -> "type constant"
    | N_Module -> "module"
  in
  let module_kind = function
    | MKStruct  -> "structure"
    | MKFunctor -> "functor"
  in
  match err with
  | OperationRedefinition(pos, name) ->
    error_p pos
      ( Box.textfl
        "Operation `%s' is defined more than once."
        name)
  | ADTCtorRedefinition(pos, name) ->
    error_p pos
      ( Box.textfl
        "Constructor `%s' is defined more than once."
        name)
  | FieldRedeclaration(pos, name) ->
    error_p pos
      ( Box.textfl
        "Field `%s' is declared more than once."
        name)
  | RecFunctionRedefinition(pos, name) ->
    error_p pos
      ( Box.textfl
        "Function `%s' is defined more than once."
        name)
  | FieldRedefinition(pos, name) ->
    error_p pos
      ( Box.textfl
        "Field `%s' is defined more than once."
        name)
  | TypeRedefinition(pos, name) ->
    error_p pos
      ( Box.textfl
        "Type `%s' is defined more than once."
        name)
  | TVarNotAllowed pos ->
    error_p pos
      (Box.textl "Syntax error: type variables are not allowed here.")
  | PatternExpected pos ->
    error_p pos (Box.textl "Syntax error: pattern was expected here.")
  | PolymorphicComputation pos ->
    error_p pos (Box.textl "Abstracting types in a non-value expressions.")
  | IllegalAbstraction pos ->
    error_p pos (Box.textl
      "Syntax error: types and values cannot be abstracted here.")
  | MissingRecFuncArg pos ->
    error_p pos (Box.textl
      "Syntax error: Recursive function without any argument.")
  | NotInfixCtor(pos, name) ->
    error_p pos
      (Box.textfl "Operator `%s' is not defined as constructor." name)
  | ValueNotProvided(pos, dpos, name) ->
    error_p pos
      (Box.textfl
        ("Module does not match its signature:"
        ^^ " value `%s' declared at %s is required, but not provided.")
        name (Utils.Position.to_string dpos))
  | TypeNotProvided(pos, dpos, name) ->
    error_p pos
      (Box.textfl
        ("Module does not match its signature:"
        ^^ " type `%s' declared at %s is required, but not provided.")
        name (Utils.Position.to_string dpos))
  | UnknownExtern(pos, name) ->
    error_p pos
      (Box.textfl "External value `%s' is not known by the runtime." name)
  | Unbound(pos, ob, name) ->
    error_p pos (Box.textfl "Unbound %s `%s'." (name_of ob) name)
  | NotModuleMember(pos, ob, name, mname) ->
    error_p pos (Box.textfl "%s `%s' is not a member of %s."
      (name_of ob) name mname)
  | TypeElemsNotVisible(pos, name) ->
    error_p pos (Box.textfl "Members of type `%s' are not visible." name)
  | ModuleKindMismatch(pos, k1, k2) ->
    error_p pos (Box.textfl
      "Module kind mismatch: found %s where %s was expected."
      (module_kind k1) (module_kind k2))
  | FunctorWithoutValType pos ->
    error_p pos (Box.textl
      ( "This module has not type annotation and "
      ^ "cannot be a result of a functor."))
  | UnboundModuleNP name ->
    error (Box.textfl "Unbound module `%s'." name)
  | DependencyLoop(path, paths) ->
    error
      ( Box.textl "Dependency loop: file"
      @ [ Box.ws (Box.word path) ]
      @ Box.textl "is required by the following files:"
      @ [ Box.suffix
        (Box.indent 4 (Box.box
          (List.map (fun path -> Box.ws (Box.word path)) paths)))
        (Box.word ".") ])

let pretty_type_mismatch_reason reason =
  match reason with
  | None -> []
  | Some c ->
    Box.textfl "Unification causes %s to escape its scope."
      (Lang.Unif.TConst.name c)

let pretty_type_error err =
  let open Transform.Flat.TypeInference in
  match err with
  | KindMismatch(pos, k1, k2) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This type expression has kind"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k1)
            (Box.word ",")) ]
      @ Box.textl "but a type expression was expected of kind"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k2)
            (Box.word ".")) ])
  | TArgKindMismatch(pos, k1, k2) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This type parameter has kind"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k1)
            (Box.word ",")) ]
      @ Box.textl "but a parameter was expected of kind"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k2)
            (Box.word ".")) ])
  | TypeMismatch(pos, Expression, tp1, tp2, reason) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This expression has type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp1)
            (Box.word ",")) ]
      @ Box.textl "but an expression was expected of type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp2)
            (Box.word ".")) ]
      @ pretty_type_mismatch_reason reason)
  | TypeMismatch(pos, ExpressionEffRow, eff1, eff2, reason) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This expression has effects"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 eff1)
            (Box.word ",")) ]
      @ Box.textl "but an expression was expected of effects"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 eff2)
            (Box.word ".")) ]
      @ pretty_type_mismatch_reason reason)
  | TypeMismatch(pos, Pattern, tp1, tp2, reason) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This pattern has type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp2)
            (Box.word ",")) ]
      @ Box.textl "but a pattern was expected of type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp1)
            (Box.word ".")) ]
      @ pretty_type_mismatch_reason reason)
  | TypeMismatch(pos, Handler, l1, l2, reason) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This operation belongs to effect"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 l1)
            (Box.word ",")) ]
      @ Box.textl "but a handler of"
      @ [ Box.ws (Unif.pretty_type tvar_env 0 l2) ]
      @ Box.textl "was expected."
      @ pretty_type_mismatch_reason reason)
  | TypeMismatch(pos, Field, tp1, tp2, reason) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "This field belongs to type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp1)
            (Box.word ",")) ]
      @ Box.textl "but a field was expected from type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp2)
            (Box.word ".")) ]
      @ pretty_type_mismatch_reason reason)
  | ValueSigMismatch(pos, dpos, name, tp1, tp2) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textfl 
        (  "Value `%s' does not match its interface declared at %s:"
        ^^ " the value has type")
          name (Utils.Position.to_string dpos)
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp1)
            (Box.word ",")) ]
      @ Box.textl "but it was expected of type"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_type tvar_env 0 tp2)
            (Box.word ".")) ])
  | TypeSigMismatch(pos, dpos, name, k1, k2) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textfl 
        (  "Type `%s' does not match its interface declared at %s:"
        ^^ " the type has kind")
          name (Utils.Position.to_string dpos)
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k1)
            (Box.word ",")) ]
      @ Box.textl "but it was expected of kind"
      @ [ Box.ws (Box.suffix
            (Unif.pretty_kind tvar_env 0 k2)
            (Box.word ".")) ])
  | EmptyHandler pos ->
    error_p pos
      ( Box.textl "Cannot guess handled effect of an empty handler.")
  | CtorArity(pos, name, n1, n2) ->
    error_p pos
      ( Box.textfl ("Constructor `%s' expects %d arguments, "
          ^^ "but here is applied to %d of them.")
          name n1 n2)
  | OpArity(pos, name, n1, n2) ->
    error_p pos
      ( Box.textfl ("Operation `%s' expects %d arguments, "
          ^^ "but here is applied to %d of them.")
          name n1 n2)
  | CtorTypeArity(pos, name, n1, n2) ->
    error_p pos
      ( Box.textfl ("Constructor `%s' expects %d type arguments, "
          ^^ "but here is applied to %d of them.")
          name n1 n2)
  | OpTypeArity(pos, name, n1, n2) ->
    error_p pos
      ( Box.textfl ("Operation `%s' expects %d type arguments, "
          ^^ "but here is applied to %d of them.")
          name n1 n2)
  | EffectAnnotInPure pos ->
    error_p pos
      ( Box.textfl ("Effect annotation for a pure computation."))
  | PolymorphicPattern pos ->
    error_p pos ( Box.textl
      ("Pattern for polymorphic field must be variable or wildcard."))
  | NonValuePolyField(pos, name) ->
    error_p pos ( Box.textfl
      ("Field `%s' is polymorphic and must be initialized by values.")
      name)

let pretty_match_error err =
  let open Transform.Unif.ToExplicit in
  match err with
  | NonExhaustiveHandler(pos, name) ->
    error_p pos
      ( Box.textfl
        "This handler is not exhaustive. Operation `%s' is not handled."
        name)
  | NonExhaustiveMatch(pos, ev) ->
    let rec pretty_ev ev =
      match ev with
      | EVAny -> Box.word "_"
      | EVCtor(name, []) -> Box.word name
      | EVCtor(name, [ ev1; ev2 ]) when
          (String.length name > 2 && name.[0] = '(') ->
        let name = String.sub name 1 (String.length name - 2) in
        Box.box
        [ Box.prefix (Box.word "(") (pretty_ev ev1)
        ; Box.ws (Box.prefix (Box.word name)
          (Box.ws (Box.suffix (pretty_ev ev2) (Box.word ")"))))
        ]
      | EVCtor(name, evs) ->
        Box.paren (Box.box
        [ Box.word name
        ; Box.box (List.map (fun ev -> Box.ws (pretty_ev ev)) evs)
        ])
      | EVRecord [] -> Box.word "_"
      | EVRecord((f, ev) :: fs) -> Box.box
        ( Box.box
          [ Box.prefix (Box.word "{")
            (Box.ws (Box.suffix (Box.word f) (Box.ws (Box.word "="))))
          ; Box.indent 2 (Box.ws (pretty_ev ev))
          ]
        ::List.map (fun (f, ev) ->
          Box.box
          [ Box.prefix (Box.word ";")
            (Box.ws (Box.suffix (Box.word f) (Box.ws (Box.word "="))))
          ; Box.indent 2 (Box.ws (pretty_ev ev))
          ]) fs
        @ [ Box.ws (Box.word "}") ])
    in
    error_p pos
      ( Box.textl "This pattern matching is not exhaustive. E.g., the value"
      @ [ Box.ws (pretty_ev ev) ]
      @ Box.textl "is not matched.")
  | NonExhaustiveRecord(pos, fields) ->
    error_p pos
      ( Box.textl
        ( "The definition of a record is not complete. "
        ^ "The following fields are not defined:")
      @ List.map (fun f -> Box.ws (Box.word f)) fields)
  | NonEmptyType(pos, tp) ->
    let tvar_env = Unif.create_tvar_env () in
    error_p pos
      ( Box.textl "Empty pattern-matching of type"
      @ [ Box.ws (Unif.pretty_type tvar_env 0 tp) ]
      @ Box.textl "which is not known to be an empty type.")
