
module Tag : sig
  type t

  val create :
    ?cmd_flag:  string ->
    ?cmd_descr: string ->
    string -> t

  val name : t -> string
end
type tag = Tag.t

module Node : sig
  type 'p t

  val create :
    ?cmd_flag:  string ->
    ?cmd_descr: string ->
      string -> 'p t
end
type 'p node = 'p Node.t

type rule

type flow_error =
| PathNotFound         of tag
| UnsupportedExtension of string

exception Flow_error of flow_error

val proc_fname  : tag -> string -> unit
val proc_stream : tag -> in_channel -> unit
val proc_pretty_node : 'a node -> 'a -> unit

val cmd_args_options : unit ->
  (Arg.key * Arg.spec * Arg.doc) list

val register_transform :
  ?require:   tag list ->
  ?contracts: tag list ->
  ?preserves: tag list ->
  ?save_tags: bool ->
  ?rules:     rule list ->
  ?weight:    float ->
  source: 'a node ->
  target: 'b node ->
    ('a -> 'b) -> unit

val register_parser :
  extension: string ->
  source:    string ->
  target:    'p node ->
    (string -> 'p) -> unit

val register_repl :
  target: 'p node ->
    (in_channel -> 'p) -> unit

val register_box_printer :
  source: 'a node ->
    ('a -> Box.t) -> unit
