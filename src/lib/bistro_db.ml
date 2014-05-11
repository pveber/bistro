open Core.Std

type t = string

let cache_dir base = Filename.concat base "cache"
let build_dir base = Filename.concat base "build"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"
let log_dir base = Filename.concat base "logs"
let history_dir base = Filename.concat base "history"

let well_formed_db path =
  if Sys.file_exists_exn path then (
    Sys.file_exists_exn (cache_dir path)
    && Sys.file_exists_exn (build_dir path)
    && Sys.file_exists_exn (tmp_dir path)
    && Sys.file_exists_exn (stderr_dir path)
    && Sys.file_exists_exn (stdout_dir path)
    && Sys.file_exists_exn (log_dir path)
    && Sys.file_exists_exn (history_dir path)
  )
  else false

let init base =
  if Sys.file_exists_exn base
  then
    if not (well_formed_db base)
    then invalid_argf "Bistro_db.init: the path %s is not available for a bistro database" base ()
  else (
    Unix.mkdir_p (tmp_dir base) ;
    Unix.mkdir_p (build_dir base) ;
    Unix.mkdir_p (cache_dir base) ;
    Unix.mkdir_p (stderr_dir base) ;
    Unix.mkdir_p (stdout_dir base) ;
    Unix.mkdir_p (log_dir base) ;
    Unix.mkdir_p (history_dir base)
  ) ;
  base

let aux_path f db w =
  Filename.concat (f db) (Bistro_workflow.digest w)

let log_path db w = aux_path log_dir db w
let build_path db w = aux_path build_dir db w
let tmp_path db w = aux_path tmp_dir db w
let stdout_path db w = aux_path stdout_dir db w
let stderr_path db w = aux_path stderr_dir db w

let rec path db = Bistro_workflow.(function
    | Input p -> p
    | Select (dir, p) ->
      Filename.concat (path db dir) p
    | Rule r as w -> aux_path cache_dir db w
  )

module Log_msg = struct
  type kind = [
    | `debug
    | `info
    | `warning
    | `error
    | `workflow_start of Bistro_workflow.u
    | `workflow_end of Bistro_workflow.u
    | `workflow_error of Bistro_workflow.u ]
  type t = {
    kind : kind ;
    contents : string ;
    time : Core.Time.t ;
  }

  let make kind fmt =
    let open Printf in
    let f msg = {
      kind ;
      time = Core.Time.now () ;
      contents = msg
    }
    in
    ksprintf f fmt

  let string_of_kind = function
    | `info -> "INFO"
    | `debug -> "DEBUG"
    | `warning -> "WARNING"
    | `error -> "ERROR"
    | `workflow_start u -> sprintf "STARTED %s" (Bistro_workflow.digest u)
    | `workflow_end u -> sprintf "FINISHED %s" (Bistro_workflow.digest u)
    | `workflow_error u -> sprintf "ERROR on %s" (Bistro_workflow.digest u)

  let to_string msg =
    let open Unix in
    sprintf
      "[%s][%s] %s"
      (string_of_kind msg.kind) (Time.format msg.time "%Y-%m-%d %H:%M:%S") msg.contents
end

let echo ~path msg =
  Out_channel.with_file ~append:true path ~f:(Fn.flip output_string msg)

let log ?hook db kind fmt =
  let msg = Log_msg.make kind fmt in
  let () = match hook with
    | None -> ()
    | Some f -> f msg
  in
  let path =
    Filename.concat
      (log_dir db)
      (Time.format (Time.now ()) "%Y-%m-%d.log")
  in
  echo ~path (Log_msg.to_string msg)
