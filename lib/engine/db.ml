(* FIXME: implement prefix subdirs to avoid too many objects in one
   dir, like aa/aa545 *)
open Core
open Rresult

module W = Bistro_internals.Workflow

type id = string

type 'a result = ('a, R.msg) Rresult.result

let ok_exn = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let filter_errors xs =
  List.filter_map xs ~f:(function
      | Ok _ -> None
      | Error e -> Some e
    )

let ( / ) = Filename.concat

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


(* Implementation of tables *)

let open_dbm ?(mode = Dbm.[ Dbm_create ; Dbm_rdwr ]) path =
  match Dbm.opendbm path mode 0o700 with
  | dbh -> Ok dbh
  | exception _ ->
    R.error_msgf "Cannot read DBM database at %s" path

let with_dbm ?mode path f =
  match open_dbm ?mode path with
  | Ok dbh ->
    Exn.protectx dbh ~f ~finally:Dbm.close
    |> R.ok
  | Error _ as e -> e


type t = string

module type Key = sig
  type t
  val to_string : t -> string
end

module type Value = sig
  type t
  val id : string
  val to_string : t -> string
  val of_string : string -> t
end

module Table(K : Key)(V : Value) :
sig
  type nonrec db = t
  val check : db -> unit result
  val create : db -> unit result
  val get : db -> K.t -> V.t option
  val set : db -> K.t -> V.t -> unit
  val fold :
    db ->
    init:'a ->
    f:('a -> V.t -> 'a) ->
    'a
end
=
struct
  type nonrec db = t
  let prefix db =
    Filename.concat db V.id

  let check db =
    let open R in
    let p = prefix db in
    check_path `File (p ^ ".pag") >>= fun () ->
    check_path `File (p ^ ".dir")

  let m = Mutex.create ()

  let with_dbm db f =
    Mutex.critical_section m ~f:(fun () ->
        with_dbm (prefix db) f
      )

  let create db =
    with_dbm db (const ())

  let get db key =
    with_dbm db (fun dbh ->
        match Dbm.find dbh (K.to_string key) with
        | value -> Some (V.of_string value)
        | exception Caml.Not_found -> None
      )
    |> ok_exn

  let set db key value =
    with_dbm db (fun dbh ->
        Dbm.replace dbh (K.to_string key) (V.to_string value)
      )
    |> ok_exn

  let fold db ~init ~f =
    with_dbm db (fun dbh ->
        let res = ref init in
        let f _ data =
          res := f !res (V.of_string data)
        in
        Dbm.iter f dbh ;
        !res
      )
    |> ok_exn
end

module Workflow_as_key = struct
  type t = W.any
  let to_string (W.Any w) = W.id w
end

module Workflow_info = struct
  let id = "workflow_info"
  type kind =
    | Pure | App | Both | List | Eval_path | Spawn | List_nth | Glob
    | Input | Select | Plugin | Shell
  [@@deriving sexp]

  type t = {
    id : string ;
    kind : kind ;
    descr : string ;
    deps : string list ;
  }
  [@@deriving sexp]

  let to_string x = Sexp.to_string (sexp_of_t x)
  let of_string x = t_of_sexp (Sexp.of_string x)

  let make ?(descr = "") ?(deps = []) kind id =
    { id ; descr ; deps ; kind }

  let kind_of_workflow (W.Any w) = match w with
    | Pure _ -> Pure
    | App _ -> App
    | Both _ -> Both
    | List _ -> List
    | Eval_path _ -> Eval_path
    | Spawn _ -> Spawn
    | List_nth _ -> List_nth
    | Input _ -> Input
    | Select _ -> Select
    | Shell _ -> Shell
    | Glob _ -> Glob
    | Plugin _ -> Plugin

  let of_workflow w =
    make
      ~deps:(List.map (W.Any.deps w) ~f:W.Any.id)
      ~descr:(Option.value (W.Any.descr w) ~default:"")
      (kind_of_workflow w)
      (W.Any.id w)
end

module Workflow_registration_table = Table(Workflow_as_key)(Workflow_info)

let cache_dir base = base / "cache"
let build_dir base = base / "build"
let tmp_dir base = base / "tmp"
let stderr_dir base = base / "stderr"
let stdout_dir base = base / "stdout"
let singularity_image_dir base = base / "singularity_image"

let get_obj f db id =
  Filename.concat (f db) id

let cache = get_obj cache_dir
let build = get_obj build_dir
let tmp = get_obj tmp_dir
let stdout = get_obj stdout_dir
let stderr = get_obj stderr_dir

let create_db path =
  let open R in
  Unix.mkdir_p (tmp_dir path) ;
  Unix.mkdir_p (build_dir path) ;
  Unix.mkdir_p (cache_dir path) ;
  Unix.mkdir_p (stderr_dir path) ;
  Unix.mkdir_p (stdout_dir path) ;
  Unix.mkdir_p (singularity_image_dir path) ;
  Workflow_registration_table.create path >>= fun () ->
  Ok ()

let dir_is_empty path =
  match Sys.readdir path with
  | [||] -> true
  | _ -> false

let no_such_path_error path =
  R.error_msgf "Path %s doesn't exist, is not readable or writable" path

(* [check_path sort p] checks that [p] exists and is of the right
   sort *)
let check_path sort p =
  match Sys.file_exists p with
  | `Yes ->
    (match sort with
    | `Dir ->
      (match Sys.is_directory p with
       | `Yes -> Ok ()
       | `Unknown | `No -> R.error_msgf "Path %s should be a directory" p)
    | `File ->
      (match Sys.is_file p with
       | `Yes -> Ok ()
       | `Unknown | `No -> R.error_msgf "Path %s should be a file" p))
  | `Unknown | `No ->
    no_such_path_error p

let dirs_of_db_exist path =
  let dir_paths = [
    path ;
    cache_dir path ;
    build_dir path ;
    tmp_dir path ;
    stderr_dir path ;
    stdout_dir path ;
    singularity_image_dir path ;
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
  let open Rresult in
  dirs_of_db_exist path >>= fun () ->
  Workflow_registration_table.check path

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

let rec path : t -> W.path -> string = fun db p ->
  match p with
  | FS_path x -> x
  | Cache_id id -> cache db id
  | Cd (dir, sel) ->
    Filename.concat (path db dir) (Path.to_string sel)

let rec workflow_path db (W.Any w) =
  let open W in
  match w with
  | Input { path ; _ } -> Some (FS_path (Misc.absolutize path))
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
      match Sys.file_exists (path db u) with
      | `Yes -> true
      | `Unknown | `No -> false)

let container_image_identifier img =
  let f account name tag =
    sprintf "%s_%s%s_%s.sif" account name
      (Option.value_map tag ~default:"" ~f:(( ^ ) "_"))
      (W.digest img)
  in
  match (img : Bistro_internals.Command.container_image) with
  | Docker_image i -> f i.account i.name i.tag
  | Singularity_image i -> f i.account i.name i.tag

let singularity_image db img =
  Filename.concat (singularity_image_dir db) (container_image_identifier img)

let register_workflow db w =
  let module T = Workflow_registration_table in
  match T.get db w with
  | Some _ -> ()
  | None -> T.set db w (Workflow_info.of_workflow w)
