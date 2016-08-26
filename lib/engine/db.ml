open Core.Std
open Rresult
open Lwt
open Bistro

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

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s

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



type t = string
type db = t




(* Implementation of tables *)

let open_dbm ?(mode = Dbm.[ Dbm_create ; Dbm_rdwr ]) path =
  match Dbm.opendbm path mode 0o700 with
  | dbh -> Ok dbh
  | exception exn ->
    R.error_msgf "Cannot read DBM database at %s" path

let with_dbm ?mode path f =
  match open_dbm ?mode path with
  | Ok dbh ->
    Exn.protectx dbh ~f ~finally:Dbm.close
    |> R.ok
  | Error _ as e -> e


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
        | exception Not_found -> None
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
        let f key data =
          res := f !res (V.of_string data)
        in
        Dbm.iter f dbh ;
        !res
      )
    |> ok_exn
end




module Task_as_value = struct
  type t = Task.t
  let id = "task"
  let to_string s =
    Sexp.to_string (Task.sexp_of_t s)
  let of_string s =
    Task.t_of_sexp (Sexp.of_string s)
end

module Task_table = struct
  include Table(String)(Task_as_value)
  let save db task = set db task.Bistro.Task.id task
end

module Task_as_key = struct
  type t = Task.t
  let to_string s = s.Task.id
end

module Stats = struct
  type t = {
    step_id : string ;
    history : (Time.t * event) list ;
    build_time : float option ;
  }
  and event = Built | Requested
  with sexp

  let to_string x =
    Sexp.to_string (sexp_of_t x)

  let of_string x =
    t_of_sexp (Sexp.of_string x)

  let id = "stats"

  let make s = {
    step_id = s.Task.id ;
    history = [] ;
    build_time = None ;
  }
end

module Stats_table = Table(Task_as_key)(Stats)


module Wave = struct
  type t = {
    name : string ;
    description : string ;
    targets : Task.t list ;
  }
  with sexp

  let to_string x =
    Sexp.to_string (sexp_of_t x)

  let of_string x =
    t_of_sexp (Sexp.of_string x)

  let id = "waves"
end

module Wave_table = Table(String)(Wave)


module Submitted_script = struct
  type t = string
  let to_string x = x
  let of_string x = x
  let id = "scripts"
end

module Submitted_script_table = Table(Task_as_key)(Submitted_script)

(* Database initialization and check *)


let cache_dir base = Filename.concat base "cache"
let build_dir base = Filename.concat base "build"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"


let check_dirs_of_db_exist path =
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


let well_formed_db path =
  let open Rresult in
  check_dirs_of_db_exist path >>= fun () ->
  Stats_table.check path >>= fun () ->
  Submitted_script_table.check path >>= fun () ->
  Wave_table.check path

let create_db path =
  let open R in
  Unix.mkdir_p (tmp_dir path) ;
  Unix.mkdir_p (build_dir path) ;
  Unix.mkdir_p (cache_dir path) ;
  Unix.mkdir_p (stderr_dir path) ;
  Unix.mkdir_p (stdout_dir path) ;
  Stats_table.create path >>= fun () ->
  Submitted_script_table.create path >>= fun () ->
  Wave_table.create path

let dir_is_empty path =
  Sys.readdir path = [||]

let ensure_path_has_db path =
  let open R in
  R.reword_error_msg
    (fun _ -> R.msg "Failed to obtain a valid bistro database")
    (
      match Sys.file_exists path with
      | `Yes ->
        if dir_is_empty path then
          create_db path
        else
          well_formed_db path
      | `No -> create_db path
      | `Unknown ->
        no_such_path_error path
    )

let init path =
  let open Rresult in
  let path =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  ensure_path_has_db path
  >>| fun () -> path


let init_exn path = ok_exn (init path)

module Task = struct
  let aux_path f db t =
    Filename.concat (f db) t.Task.id

  let cache_path = aux_path cache_dir
  let build_path = aux_path build_dir
  let tmp_path = aux_path tmp_dir
  let stdout_path = aux_path stdout_dir
  let stderr_path = aux_path stderr_dir

  let update_stats db step f =
    let stat =
      match Stats_table.get db step with
      | Some s -> s
      | None -> Stats.make step
    in
    Stats_table.set db step (f stat)

  let append_history ~db u evt =
    update_stats db u (fun stat ->
        { stat with
          Stats.history = (Time.now (), evt) :: stat.Stats.history }
      )

  let requested db step =
    append_history ~db step Stats.Requested

  let built db step =
    append_history ~db step Stats.Built

  let in_cache db u =
    let dest = cache_path db u in
    Sys.file_exists dest = `Yes
end

let report db task =
  let buf = Buffer.create 251 in
  let script = Submitted_script_table.get db task in
  bprintf buf "################################################################################\n" ;
  bprintf buf "###\n" ;
  bprintf buf "##    Report on %s \n" task.Bistro.Task.id ;
  bprintf buf "#\n" ;
  Option.iter script ~f:(fun script ->
      bprintf buf "+------------------------------------------------------------------------------+\n" ;
      bprintf buf "| Submitted script                                                             |\n" ;
      bprintf buf "+------------------------------------------------------------------------------+\n" ;
      bprintf buf "%s\n" script
    ) ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDOUT                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s\n" (In_channel.read_all (Task.stdout_path db task)) ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDERR                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s\n" (In_channel.read_all (Task.stderr_path db task)) ;
  Buffer.contents buf


let output_report db t oc = output_string oc (report db t)

let register_workflow db w =
  let tasks = Bistro.Task.decompose_workflow w in
  String.Map.iter tasks ~f:(fun ~key ~data -> Task_table.set db key data)

let register_workflows db ws =
  let tasks_by_workflow = List.map ws ~f:Bistro.Task.decompose_any_workflow in
  let merge x y = String.Map.merge x y ~f:(fun ~key -> function
        `Left t -> Some t
      | `Right t -> Some t
      | `Both (t1, t2) ->
        assert (t1 = t2) ;
        Some t1
    )
  in
  let all_tasks = List.fold tasks_by_workflow ~init:String.Map.empty ~f:merge in
  String.Map.iter all_tasks ~f:(fun ~key ~data -> Task_table.set db key data)


let workflow_path db w =
  match Bistro.Task.classify_workflow w with
  | `Input i -> string_of_path i
  | `Select (id, p) ->
    List.reduce_exn
      [ cache_dir db ; id ; string_of_path p ]
      ~f:Filename.concat
  | `Task tid ->
    Filename.concat (cache_dir db) tid

(* let log db fmt = *)
(*   let f msg = *)
(*     let path = *)
(*       Filename.concat *)
(*         (log_dir db) *)
(*         (Time.format (Time.now ()) "%Y-%m-%d.log") *)
(*     in *)
(*     echo ~path msg *)
(*   in *)
(*   Printf.ksprintf f fmt *)
