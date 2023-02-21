
type 'a t =
  { file_map : (string, 'a) Hashtbl.t
  ; req_by   : string list
  ; cur_dir  : string option
  ; src_dirs : string list
  ; lib_dirs : string list
  ; mpath    : string list
  ; tr_file  : 'a t -> mpath:string list -> ?intf:string -> string -> 'a
  }

let path_concat dir name =
  if dir = Filename.current_dir_name then name
  else Filename.concat dir name

let canonical_fname fname =
  if Filename.is_relative fname then
    path_concat (Filename.dirname fname) (Filename.basename fname)
  else fname

let init ?current_path ?(source_dirs=[]) ?(lib_dirs=[]) tr_file =
  let req_by =
    match current_path with
    | None      -> []
    | Some path -> [ canonical_fname path ]
    in
  let cur_dir =
    match current_path with
    | None      -> None
    | Some path -> Some (Filename.dirname path)
  in
  { file_map = Hashtbl.create 32
  ; req_by   = req_by
  ; cur_dir  = cur_dir
  ; src_dirs = source_dirs
  ; lib_dirs = lib_dirs
  ; mpath    = []
  ; tr_file  = tr_file
  }

let update finder
    ?current_path
    ?(source_dirs=finder.src_dirs)
    ?(lib_dirs=finder.lib_dirs) tr_file =
  let req_by =
    match current_path with
    | None      -> finder.req_by
    | Some path -> [ canonical_fname path ]
    in
  let cur_dir =
    match current_path with
    | None      -> finder.cur_dir
    | Some path -> Some (Filename.dirname path)
  in
  { file_map = finder.file_map
  ; req_by   = req_by
  ; cur_dir  = cur_dir
  ; src_dirs = source_dirs
  ; lib_dirs = lib_dirs
  ; mpath    = finder.mpath
  ; tr_file  = tr_file
  }

let find_loop path req_by =
  let rec aux acc req_by =
    match req_by with
    | [] -> None
    | path' :: req_by ->
      if path = path' then Some (List.rev (path' :: acc))
      else aux (path' :: acc) req_by
  in aux [] req_by

type 'a find_result =
| Cached of 'a
| Exists of string
| NotFound

let find_in_dir file_map dir fname =
  let path = path_concat dir fname in
  match Hashtbl.find_opt file_map path with
  | Some r -> Cached r
  | None   ->
    if Sys.file_exists path && not (Sys.is_directory path) then
      Exists path
    else NotFound

let rec find_in_dirs file_map dirs fname =
  match dirs with
  | [] -> NotFound
  | dir :: dirs ->
    begin match find_in_dir file_map dir fname with
    | Cached r    -> Cached r
    | Exists path -> Exists path
    | NotFound    -> find_in_dirs file_map dirs fname
    end

let find_in_dir_opt file_map dir fname =
  match dir with
  | None     -> NotFound
  | Some dir -> find_in_dir file_map dir fname

let tr_file finder name path src_dirs =
  match find_loop path finder.req_by with
  | Some loop -> raise (Errors.dependency_loop path loop)
  | None ->
    let finder =
      { finder with
        req_by   = path :: finder.req_by
      ; cur_dir  = Some (Filename.dirname path)
      ; src_dirs = src_dirs
      ; mpath    = [ name ]
      }
    in
    let base_path = Filename.chop_suffix path (Filename.extension path) in
    let intf_path = base_path ^ ".she" in
    let intf_path =
      if Sys.file_exists intf_path then Some intf_path
      else None in
    let r = finder.tr_file finder ~mpath:finder.mpath ?intf:intf_path path in
    Hashtbl.add finder.file_map path r;
    r

let find finder name =
  let fname = name ^ ".he" in
  match find_in_dir_opt finder.file_map finder.cur_dir fname with
  | Cached r    -> Some r
  | Exists path -> Some (tr_file finder name path finder.src_dirs)
  | NotFound ->
    begin match find_in_dirs finder.file_map finder.src_dirs fname with
    | Cached r -> Some r
    | Exists path -> Some (tr_file finder name path finder.src_dirs)
    | NotFound ->
      begin match find_in_dirs finder.file_map finder.lib_dirs fname with
      | Cached r -> Some r
      | Exists path -> Some (tr_file finder name path [])
      | NotFound -> None
      end
    end
