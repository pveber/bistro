open Core.Std
open Rresult
open Lwt
open Bistro
open Bistro.Workflow

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s


type 'a result = ('a, R.msg) Rresult.result


type t = {
  path : string ;
  dbh : Dbm.t ; (* DBM handler *)
}

let cache_dir base = Filename.concat base "cache"
let build_dir base = Filename.concat base "build"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"
let stats_prefix base = Filename.concat base "stats"

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

let no_such_path_error path =
  R.error_msgf "Path %s doesn't exist, is not readable or writable" path

let check_path (p, sort) =
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

let check_dbm_open path =
  with_dbm path (const ())

let check_paths_of_db_exist path =
  let paths = [
      path, `Dir ;
      cache_dir path, `Dir ;
      build_dir path, `Dir ;
      tmp_dir path, `Dir ;
      stderr_dir path, `Dir ;
      stdout_dir path, `Dir ;
      (stats_prefix path) ^ ".pag", `File ;
      (stats_prefix path) ^ ".dir", `File ;
    ]
  in
  let msgs = List.filter_map paths ~f:(fun p ->
      match check_path p with
      | Ok () -> None
      | Error msg -> Some msg
    )
  in
  if msgs = [] then Ok ()
  else
    let format_msgs fmt =
      List.map msgs ~f:(fun (`Msg msg) -> "\t" ^ msg)
      |> String.concat ~sep:"\n"
      |> Format.pp_print_string fmt
    in
    R.error_msgf "Malformed database at %s:\n%t" path format_msgs

let well_formed_db path =
  let open Rresult in
  check_paths_of_db_exist path >>= fun () ->
  check_dbm_open path

let ensure_path_has_db path =
  R.reword_error_msg
    (fun _ -> R.msg "Failed to obtain a valid bistro database")
    (
      match Sys.file_exists path with
      | `Yes ->
        well_formed_db path
      | `No ->
        Unix.mkdir_p (tmp_dir path) ;
        Unix.mkdir_p (build_dir path) ;
        Unix.mkdir_p (cache_dir path) ;
        Unix.mkdir_p (stderr_dir path) ;
        Unix.mkdir_p (stdout_dir path) ;
        check_dbm_open path
      | `Unknown ->
        no_such_path_error path
    )

let open_ path =
  let open Rresult in
  let path =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  ensure_path_has_db path
  >>= fun () -> open_dbm path
  >>| fun dbh -> { path ; dbh }


let ok_exn = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let open_exn path = ok_exn (open_ path)

let close db = Dbm.close db.dbh

let with_open path f =
  let open Lwt in
  match open_ path with
  | Ok db ->
    finalize
      (fun () -> f db >|= fun x -> Ok x)
      (fun () -> close db ; return ())
  | Error e -> return (Error e)

let with_open_exn path f =
  let open Lwt in
  with_open path f >|= ok_exn

let aux_path f db step =
  Filename.concat (f db.path) step.id

let cache_path = aux_path cache_dir
let build_path = aux_path build_dir
let tmp_path = aux_path tmp_dir
let stdout_path = aux_path stdout_dir
let stderr_path = aux_path stderr_dir

module Stats = struct
  type t = {
    workflow : step ;
    history : (Time.t * event) list ;
    build_time : float option ;
  }
  and event = Built | Requested
  with sexp

  let make s = {
    workflow = s ;
    history = [] ;
    build_time = None ;
  }

  let load db step =
    match Dbm.find db step.id with
    | sexp -> Some (t_of_sexp (Sexp.of_string sexp))
    | exception Not_found -> None

  let save db step stats =
    Dbm.replace db step.id (Sexp.to_string (sexp_of_t stats))

end

let fold db ~init ~f =
  let res = ref init in
  let f key data =
    res := f !res (Stats.t_of_sexp (Sexp.of_string data))
  in
  Dbm.iter f db.dbh ;
  !res


let update_stats db step f =
  let stat =
    match Stats.load db.dbh step with
    | Some s -> s
    | None -> Stats.make step
  in
  Stats.save db.dbh step (f stat)


let append_history ~db u evt =
  update_stats db u (fun stat ->
      { stat with
        Stats.history = (Time.now (), evt) :: stat.Stats.history }
    )

let requested db step =
  append_history ~db step Stats.Requested

let built db step =
  append_history ~db step Stats.Built

let rec workflow_path' db = function
  | Select (_, dir, p) ->
    Filename.concat (workflow_path' db dir) (string_of_path p)
  | Input (_, p) -> string_of_path p
  | Step step -> cache_path db step

let workflow_path db w = workflow_path' db (Workflow.u w)

let report_step db step =
  let buf = Buffer.create 251 in
  let dest = build_path db step in
  let tmp = tmp_path db step in
  let script = Script.to_string ~string_of_workflow:(workflow_path' db) ~dest ~tmp step.script in
  bprintf buf "################################################################################\n" ;
  bprintf buf "###\n" ;
  bprintf buf "##    Report on %s \n" step.id ;
  bprintf buf "#\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| Script                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" script ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDOUT                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" (In_channel.read_all (stdout_path db step)) ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDERR                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" (In_channel.read_all (stderr_path db step)) ;
  Buffer.contents buf

let report db = function
  | Input _ -> ""
  | Select _ -> ""
  | Step step -> report_step db step

let output_report db u oc = match u with
  | Input _ -> ()
  | Select _ -> ()
  | Step step -> output_string oc (report_step db step)

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
