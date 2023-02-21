open AbstractMachine

let rec pretty_value v =
  match v with
  | VNum n    -> Box.word (string_of_int n)
  | VChar c   -> Box.word ("'" ^ Char.escaped c ^ "'")
  | VString s -> Box.word ("\"" ^ String.escaped s ^ "\"")
  | VClo _    -> Box.word "<fun>"
  | VCont _   -> Box.word "<cont>"
  | VPrim _   -> Box.word "<prim>"
  | VEffect _ -> Box.word "<effect>"
  | VCoercion _ -> Box.word "<coercion>"
  | VSeal _   -> Box.word "<abstr>"
  | VCtor(n, []) -> Box.word ("@" ^ string_of_int n)
  | VCtor(n, vs) -> Box.paren (Box.box
    [ Box.word ("@" ^ string_of_int n)
    ; Box.indent 2 (Box.box
        (List.map (fun v -> Box.ws (pretty_value v)) vs))
    ])

let _ =
  Flow.register_box_printer 
    ~source:pretty_flow_node
    pretty_value
