
val pretty_flow_error : Flow.flow_error -> Box.t

val pretty_parse_error : Parser.parse_error -> Box.t

val pretty_scope_error : Transform.Raw.ScopeCheck.scope_error -> Box.t

val pretty_type_error : Transform.Flat.TypeInference.type_error -> Box.t

val pretty_match_error : Transform.Unif.ToExplicit.error -> Box.t
