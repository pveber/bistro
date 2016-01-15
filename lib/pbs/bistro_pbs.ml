let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )
module Lwt' = Pvem.With_deferred(Lwt)

type result = Pbs.Qstat.t
type error = [
  | `Failure of string
  | `Qsub_failure of string * int
  | `Qstat_failure of string * int
  | `Qstat_wrong_output of string
]

let pread ?timeout ?env ?stdin cmd =
  let pr = Lwt_process.open_process_full cmd in
  ignore (Lwt_io.close pr#stdin) ;
  Lwt.finalize
    (fun () ->
       Lwt_io.read pr#stdout >>= fun stdout ->
       Lwt_io.read pr#stderr >>| fun stderr ->
       stdout, stderr
    )
    (fun () ->
       Lwt_io.close pr#stdout >>= fun () ->
       Lwt_io.close pr#stderr)
  >>= fun (stdout, stderr) ->
  pr#close >>| fun exit_status ->
  stdout, stderr, exit_status

let trim_newline s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then
    String.sub s 0 (n - 1)
  else
    s

let qsub queue script =
  let fn = Filename.temp_file "tmp" ".pbs" in
  Lwt_io.(
    with_file ~mode:Output fn (fun oc ->
        write oc (Pbs.Script.to_string script)
      )
  ) >>= fun () ->
  let args = [| "-q " ^ queue ; fn |] in
  let cmd = ("qsub", args) in
  pread ~stdin:`Close cmd  >>| function
  | stdout, _, Unix.WEXITED 0 ->
    let job_id = trim_newline stdout in
    `Ok job_id
  | _, stderr, (Unix.WEXITED code | Unix.WSIGNALED code | Unix.WSTOPPED code) ->
    `Error (`Qsub_failure (trim_newline stderr, code))

let qstat job_id =
  let cmd = ("", [| "qstat" ; "-f1" ; job_id |]) in
  pread ~stdin:`Close cmd  >>| function
  | stdout, _, Unix.WEXITED 0 -> (
      match Pbs.Qstat.parse_qstat stdout with
      | `Ok r -> `Ok r
      | `Error e -> `Error (`Qstat_wrong_output (Pbs.Qstat.error_to_string e))
    )

  | _, stderr, (Unix.WEXITED code | Unix.WSIGNALED code | Unix.WSTOPPED code) ->
    `Error (`Qstat_failure (trim_newline stderr, code))

let submit ~queue script : (_, error) Lwt'.t =
  qsub queue script >>= function
  | `Ok job_id ->
    let rec loop () =
      Lwt_unix.sleep 2. >>= fun () ->
      qstat job_id >>= function
      | `Ok stat -> (
          match Pbs.Qstat.get_status stat with
          | `Ok (`exiting | `held | `moved | `queued | `running | `suspended | `waiting) ->
            loop ()
          | `Ok `completed -> Lwt'.return stat
          | `Error e ->
            Lwt'.fail (`Qstat_wrong_output (Pbs.Qstat.error_to_string e))
        )
      | `Error e -> Lwt'.fail e
    in
    loop ()

  | `Error e -> Lwt'.fail e
