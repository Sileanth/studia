type 'a t

val init :
  ?current_path: string ->
  ?source_dirs:  string list ->
  ?lib_dirs:     string list ->
    ('a t -> mpath:string list -> ?intf:string -> string -> 'a) -> 'a t

val update : 'a t ->
  ?current_path: string ->
  ?source_dirs:  string list ->
  ?lib_dirs:     string list ->
    ('a t -> mpath:string list -> ?intf:string -> string -> 'a) -> 'a t

val find : 'a t -> string -> 'a option
