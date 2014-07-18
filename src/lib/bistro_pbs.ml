let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let hours_of_duration = function
  | `minute -> 1. /. 60.
  | `hour -> 1.
  | `day -> 24.
  | `week -> 7. *. 24.
  | `month -> 30. *. 7. *. 24.

let interpreter_path = function
  | `bash -> "/bin/bash"
  | `ocaml -> "/usr/bin/ocaml"
  | `perl -> "/usr/bin/perl"
  | `python -> "/usr/bin/python"
  | `R -> "/usr/bin/R CMD"
  | `sh -> "/bin/sh"

let pbs_header ~mem ~np ~timeout ~stderr ~stdout ~interpreter =
  let open Pbs.Script in
  create
    ~walltime:(`Hours (hours_of_duration timeout))
    ~stderr_path:stderr
    ~stdout_path:stdout
    ~ppn:np
    ~shell:(interpreter_path interpreter)
    (Program.command_sequence [])
  |> to_string


let query_job_stats job_id =
  Lwt_process.pread ("", [| "qstat" ; "-f1" ; job_id |]) >|=
  Pbs.Qstat.parse_qstat >>= function
  | `Ok r -> Lwt.return r
  | `Error msg -> Lwt.fail (Failure (Pbs.Qstat.error_to_string msg))

let rec wait_for_termination job_id =
  query_job_stats job_id >>= fun stats ->
  match Pbs.Qstat.get_status stats with
  | `Ok `completed -> (
    match Pbs.Qstat.raw_field stats "exit_status" with
    | Some s -> Lwt.return (int_of_string s)
    | None -> Lwt.fail (Failure "Bistro_pbs.wait_for_termination: no exit_status in completed job")
    )
  | `Error _ -> Lwt.fail (Failure "Bistro.wait_for_termination: failed to parse qstat output")
  | _ ->
    Lwt_unix.sleep 600. >>= fun () ->
    wait_for_termination job_id

let worker blog ~np ~mem ~timeout ~interpreter ~(stdout : string) ~stderr script =
  let ext = Bistro_workflow.extension_of_interpreter interpreter in
  let script_file = Filename.temp_file "bistro" ("." ^ ext) in
  let header = pbs_header ~mem ~np ~timeout ~stderr ~stdout ~interpreter in
  Lwt_io.(with_file ~mode:output script_file (fun oc ->
      write oc header >>= fun () ->
      write oc script
    )) >>= fun () ->
  let cmd = ("", [| "qsub" ; script_file |]) in
  Lwt_process.pread_line cmd >>= fun job_id ->
  wait_for_termination job_id >>= function
  | 0 ->
    Lwt_unix.unlink script_file >>= fun () ->
    Lwt.return `Ok
  | code -> (
    Bistro_log.error blog
      "Script %s failed!\nError code: %d\nstdout: %s\nstderr: %s\n"
      script_file code stdout stderr ;
    Lwt.return `Error
  )

