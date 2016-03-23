open Core.Std
open Lwt
open Bistro
open Bistro_engine

type task = {
  script : string ;
  script_path : string ;
  pbs_script : Pbs.Script.t ;
}

let extension_of_interpreter = function
  | `bash -> "sh"
  | `ocaml -> "ml"
  | `ocamlscript -> "ml"
  | `python -> "py"
  | `perl -> "pl"
  | `R -> "R"
  | `sh -> "sh"

let make_task
    ~workdir ~queue
    ~np ~mem ?timeout
    ~stdout ~stderr ~dest ~tmp ~string_of_workflow
    script =

  let interpreter = Script.interpreter script in
  let node_dest   = Filename.concat workdir "dest"   in
  let node_tmp    = Filename.concat workdir "tmp"    in
  let node_stdout = Filename.concat workdir "stdout" in
  let node_stderr = Filename.concat workdir "stderr" in
  let script_text =
    Script.to_string
      ~string_of_workflow
      ~np ~mem ~dest:node_dest ~tmp:node_tmp script in
  let ext = extension_of_interpreter interpreter in
  let script_path = Filename.concat tmp ("script." ^ ext) in
  let pbs_script_body =
    [
      sprintf "mkdir -p %s" node_tmp ;
      sprintf "bash %s > %s 2> %s" script_path node_stdout node_stderr ;
      "ECODE=$?" ;
      sprintf "cp %s %s" node_stdout stdout ;
      sprintf "cp %s %s" node_stderr stderr ;
      sprintf "if [ $ECODE -eq 0 ]; then if [ -e %s ]; then cp %s %s; fi fi" node_dest node_dest dest ;
      sprintf "rm -rf %s" workdir ;
      "exit $ECODE" ;
    ]
    |> String.concat ~sep:"\n"
  in
  let pbs_script =
    Pbs.Script.raw
      ~queue
      ?walltime:(Option.map timeout ~f:(fun x -> `Hours (float x)))
      ~stderr_path:"/dev/null"
      ~stdout_path:"/dev/null"
      pbs_script_body
  in
  { script = script_text ; script_path ;
    pbs_script ; }



let make ~workdir ~queue : Scheduler.backend =
  let new_id =
    let id = ref 0 in
    fun () -> incr id ; !id
  in
  fun db ({ Workflow.np ; mem ; script } as step) ->
    let stdout = Db.stdout_path db step in
    let stderr = Db.stderr_path db step in
    let dest = Db.build_path db step in
    let tmp = Db.tmp_path db step in
    let string_of_workflow = Db.workflow_path' db in
    let id = new_id () in
    let workdir = sprintf "%s/%06d" workdir id in
    let task =
      make_task
        ~np ~mem
        ~stdout ~stderr ~dest ~tmp ~string_of_workflow
        ~workdir ~queue script
    in
    Lwt_io.(with_file
              ~mode:output task.script_path
              (fun oc -> write oc task.script)) >>= fun () ->
    Client.submit ~queue task.pbs_script >>= function
    | `Error (`Failure msg) ->
      Lwt.fail (Failure ("PBS FAILURE: " ^ msg))
    | `Error (`Qsub_failure (msg, _)) ->
      Lwt.fail (Failure ("QSUB FAILURE: " ^ msg))
    | `Error (`Qstat_failure (msg, _)) ->
      Lwt.fail (Failure ("QSTAT FAILURE: " ^ msg))
    | `Error (`Qstat_wrong_output msg) ->
      Lwt.fail (Failure ("QSTAT WRONG OUTPUT: " ^ msg))
    | `Ok qstat -> (
        match Pbs.Qstat.raw_field qstat "exit_status" with
        | None -> Lwt.fail (Failure "missing exit status")
        | Some code ->
          let exit_status = int_of_string code in
          Lwt.return {
            Scheduler.script = task.script ;
            exit_status ;
          }
      )
