
module TagCore : sig
  type t
  val compare : t -> t -> int

  val create_tag : string -> t

  val name : t -> string
end = struct
  type t =
    { id   : Utils.UID.t
    ; name : string
    }

  let compare t1 t2 = Utils.UID.compare t1.id t2.id

  let create_tag name =
    { id   = Utils.UID.fresh ()
    ; name = name
    }

  let name tag = tag.name
end

type tag = TagCore.t
module TagSet = Set.Make(TagCore)

type rule_action =
| Set    of TagSet.t
| Add    of TagSet.t
| Remove of TagSet.t

type rule = TagSet.t * rule_action

type 'p node =
  {         id      : Utils.UID.t
  ;         name    : string
  ;         tag     : tag
  ; mutable outputs : tag list
  ; mutable edges   : 'p edge list
  }
and 'p edge =
| Edge :
  { require   : TagSet.t
  ; rules     : rule list 
  ; target    : 'p2 node 
  ; func      : ('p -> 'p2)
  ; weight    : float
  } -> 'p edge

type flow_error =
| PathNotFound         of tag
| UnsupportedExtension of string

exception Flow_error of flow_error

(* ========================================================================= *)

type command =
| Cmd of tag

let command_list = ref []

let get_command_list default_tgt =
  match !command_list with
  | []   -> [ Cmd default_tgt ]
  | cmds -> List.rev cmds

(* ========================================================================= *)

let cmd_args_option_list = ref []

let cmd_args_options () =
  List.rev !cmd_args_option_list

(* ========================================================================= *)

let proc_rules init_tags =
  List.fold_left (fun tags (req, act) ->
    if TagSet.subset req init_tags then
      match act with
      | Set    ts -> ts
      | Add    ts -> TagSet.union tags ts
      | Remove ts -> TagSet.diff  tags ts
    else tags)

(* ========================================================================= *)

type 'p path =
| PNil  of 'p node * TagSet.t
| PCons :  ('p -> 'p2) * 'p2 path -> 'p path

type ('p1, _) rev_path =
| RP_Nil  : ('p1, 'p1) rev_path
| RP_Cons : ('p2 -> 'p3) * ('p1, 'p2) rev_path -> ('p1, 'p3) rev_path

let rec rev_path : type p1 p2. p1 path -> (p2, p1) rev_path -> p2 path =
    fun acc path ->
  match path with
  | RP_Nil           -> acc
  | RP_Cons(f, path) -> rev_path (PCons(f, acc)) path

module Pos = struct
  type t =
  | Pos : 'a node * TagSet.t -> t

  let compare (Pos(n1, ts1)) (Pos(n2, ts2)) =
    let c0 = Utils.UID.compare n1.id n2.id in
    if c0 <> 0 then c0
    else TagSet.compare ts1 ts2
end
module PosSet = Set.Make(Pos)

exception Path_not_found

type 'p1 position =
| Position : float * 'p2 node * TagSet.t * ('p1, 'p2) rev_path -> 'p1 position

let find_path (type p) tgt (node : p node) tags : p path =
  let mk_pos node tags = Pos.Pos(node, tags) in
  let visited = ref (PosSet.singleton (mk_pos node tags)) in
  let module PosQueue = Set.Make(struct
      type t = p position
      let compare (Position(w1, n1, s1, _)) (Position(w2, n2, s2, _)) =
        if w1 < w2 then -1
        else if w1 > w2 then 1
        else Pos.compare (mk_pos n1 s1) (mk_pos n2 s2)
    end) in
  let queue = ref (PosQueue.singleton (Position(0.0, node, tags, RP_Nil))) in
  let rec find_path_rec () =
    match PosQueue.min_elt_opt !queue with
    | None -> raise Path_not_found
    | Some(Position(w, node, tags, rp) as pos) ->
      queue := PosQueue.remove pos !queue;
      if TagSet.mem tgt tags then rev_path (PNil(node, tags)) rp
      else begin
        List.iter (fun (Edge edge) ->
          if TagSet.subset edge.require tags then
            let tags2 = proc_rules tags tags edge.rules in
            let pos = mk_pos edge.target tags2 in
            if PosSet.mem pos !visited then ()
            else begin
              visited := PosSet.add pos !visited;
              queue := PosQueue.add
                (Position(w +. edge.weight, edge.target, tags2,
                  RP_Cons(edge.func, rp)))
                !queue
            end
          else ()
        ) node.edges;
        find_path_rec ()
      end
  in find_path_rec ()

type 'r exec_cont =
  { exec_cont : 'p. 'p node -> TagSet.t -> 'p -> 'r
  }

let rec exec_path : type p r. p path -> p -> r exec_cont -> r =
    fun path prog cont ->
  match path with
  | PNil(node, tags) -> cont.exec_cont node tags prog
  | PCons(f, path)   -> exec_path path (f prog) cont

let rec flow_output : type p. tag list -> p node -> TagSet.t -> p -> unit =
    fun outputs node tags prog ->
  match outputs with
  | [] -> ()
  | tag :: outputs ->
    begin match find_path tag node tags with
    | path -> exec_path path prog { exec_cont = fun _ _ _ -> () }
    | exception Path_not_found ->
      flow_output outputs node tags prog
    end

let rec flow : type p. command list -> p node -> TagSet.t -> p -> unit =
    fun cmds node tags prog ->
  match cmds with
  | [] -> flow_output node.outputs node tags prog
  | Cmd tag :: cmds ->
    begin match find_path tag node tags with
    | path -> exec_path path prog
      { exec_cont = fun node tags prog -> flow cmds node tags prog }
    | exception Path_not_found ->
      raise (Flow_error(PathNotFound tag))
    end

(* ========================================================================= *)

let extension_tab = Hashtbl.create 32

let proc_fname default_tgt fname =
  let ext = Filename.extension fname in
  match Hashtbl.find_opt extension_tab ext with
  | Some node ->
    flow (get_command_list default_tgt) node TagSet.empty fname
  | None ->
    raise (Flow_error(UnsupportedExtension ext))

(* ========================================================================= *)

let proc_pretty_node (type p) (node : p node) (v : p) : unit =
  flow_output node.outputs node TagSet.empty v

(* ========================================================================= *)

module Tag = struct
  include TagCore

  let create ?cmd_flag ?cmd_descr name =
    let tag = TagCore.create_tag name in
    begin match cmd_flag with
    | None -> ()
    | Some flag ->
      cmd_args_option_list :=
        (flag, Arg.Unit (fun () ->
            command_list := Cmd tag :: !command_list),
          begin match cmd_descr with
          | None       -> " <no description>"
          | Some descr -> descr
          end) :: !cmd_args_option_list
    end;
    tag
end

module Node = struct
  type 'p t = 'p node

  let create ?cmd_flag ?cmd_descr name =
    { id      = Utils.UID.fresh ()
    ; name    = name
    ; tag     = Tag.create ?cmd_flag ?cmd_descr name
    ; outputs = []
    ; edges   = []
    }
end

module Rule = struct
  let set_always tags = (TagSet.empty, Set(TagSet.of_list tags))
  let add_always tags = (TagSet.empty, Add(TagSet.of_list tags))

  let preserves_tag tag = (TagSet.singleton tag, Add(TagSet.singleton tag))
end

(* ========================================================================= *)

let stream_node = Node.create "Stream"

let proc_stream default_tgt chan =
  flow (get_command_list default_tgt) stream_node TagSet.empty chan

(* ========================================================================= *)

let register_source ~extension node =
  Hashtbl.add extension_tab extension node

let register_transform
  ?(require=[])
  ?(contracts=[])
  ?(preserves=[])
  ?(save_tags=false)
  ?(rules=[])
  ?(weight=1.0)
    ~source ~target func =
  let rules = rules
    @ (List.map Rule.preserves_tag preserves)
    @ [ Rule.add_always [ target.tag ]] in
  let rules =
    if not save_tags then
      Rule.set_always contracts :: rules
    else if contracts = [] then rules
    else Rule.add_always contracts :: rules
  in
  source.edges <- Edge 
    { require   = TagSet.of_list require
    ; rules     = rules
    ; target    = target
    ; func      = func
    ; weight    = weight
    } :: source.edges

let register_parser ~extension ~source ~target func =
  let source_node = Node.create source in
  register_source ~extension source_node;
  register_transform 
    ~source: source_node
    ~target: target
    func

let register_repl ~target func =
  register_transform ~source:stream_node ~target func

let pretty_node = Node.create "Pretty"

let register_box_printer ~source pretty =
  let tag = Tag.create (source.name ^ " pretty-printer") in
  source.outputs <- tag :: source.outputs;
  register_transform
    ~contracts: [ tag ]
    ~source:    source
    ~target:    pretty_node
    (fun prog -> prog |> pretty |> Box.print_stdout)
