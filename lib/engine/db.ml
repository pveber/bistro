(* FIXME: implement prefix subdirs to avoid too many objects in one
   dir, like aa/aa545 *)
open Core
open Rresult

type id = string

let ok_exn = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let filter_errors xs =
  List.filter_map xs ~f:(function
      | Ok _ -> None
      | Error e -> Some e
    )

let ( / ) = Filename.concat

type t = string

let cache_dir base = base / "cache"
let build_dir base = base / "build"
let tmp_dir base = base / "tmp"
let stderr_dir base = base / "stderr"
let stdout_dir base = base / "stdout"

let get_obj f db id =
  Filename.concat (f db) id

let cache = get_obj cache_dir
let build = get_obj build_dir
let tmp = get_obj tmp_dir
let stdout = get_obj stdout_dir
let stderr = get_obj stderr_dir

let create_db path =
  Unix.mkdir_p (tmp_dir path) ;
  Unix.mkdir_p (build_dir path) ;
  Unix.mkdir_p (cache_dir path) ;
  Unix.mkdir_p (stderr_dir path) ;
  Unix.mkdir_p (stdout_dir path) ;
  Ok ()

let dir_is_empty path =
  Sys.readdir path = [||]

let no_such_path_error path =
  R.error_msgf "Path %s doesn't exist, is not readable or writable" path

(* [check_path sort p] checks that [p] exists and is of the right
   sort *)
let check_path sort p =
  if Sys.file_exists p = `Yes then
    match sort with
    | `Dir ->
      if Sys.is_directory p = `Yes then Ok ()
      else R.error_msgf "Path %s should be a directory" p
    | `File ->
      if Sys.is_file p = `Yes then Ok ()
      else R.error_msgf "Path %s should be a file" p
  else
    no_such_path_error p

let dirs_of_db_exist path =
  let dir_paths = [
    path ;
    cache_dir path ;
    build_dir path ;
    tmp_dir path ;
    stderr_dir path ;
    stdout_dir path ;
  ]
  in
  let checks = List.map dir_paths ~f:(check_path `Dir) in
  match filter_errors checks with
  | [] -> Ok ()
  | h :: t ->
    R.reword_error_msg
      (fun _ -> `Msg (sprintf "Malformed database at %s" path))
      (Error (
          List.fold t ~init:h ~f:(fun (`Msg accu) (`Msg msg) ->
              `Msg (accu ^ "\n" ^ msg)
            )
        )
      )

let db_is_well_formed path =
  dirs_of_db_exist path

let path_has_valid_db path =
  R.reword_error_msg
    (fun _ -> R.msg "Failed to obtain a valid bistro database")
    (
      match Sys.file_exists path with
      | `Yes ->
        if dir_is_empty path then
          create_db path
        else
          db_is_well_formed path
      | `No -> create_db path
      | `Unknown ->
        no_such_path_error path
    )

let init path =
  let path =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  path_has_valid_db path
  >>| fun () -> path

let init_exn path = ok_exn (init path)

let fold_cache db ~init ~f =
  Array.fold
    (Sys.readdir (cache_dir db))
    ~init ~f

let rec path : t -> Bistro_internals.Workflow.path -> string = fun db p ->
  match p with
  | FS_path x -> x
  | Cache_id id -> cache db id
  | Cd (dir, sel) ->
    Filename.concat (path db dir) (Path.to_string sel)

let rec workflow_path db (Bistro_internals.Workflow.Any w) =
  let open Bistro_internals.Workflow in
  match w with
  | Input { path ; _ } -> Some (FS_path path)
  | Select { dir ; sel ; _ } ->
    workflow_path db (Any dir)
    |> Option.map ~f:(fun d -> Cd (d, sel))
  | Shell { id ; _ } -> Some (Cache_id id)
  | Plugin { id ; task = Path_plugin _ ; _ } -> Some (Cache_id id)
  | Plugin { id ; task = Value_plugin _ ; _ } -> Some (Cache_id id)
  | _ -> None

let is_in_cache db u =
  workflow_path db u
  |> Option.value_map ~default:false ~f:(fun u ->
      Sys.file_exists (path db u) = `Yes
    )
