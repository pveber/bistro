open Core
open Bistro
open Bistro.Shell_dsl
open Bistro_engine
open Bistro_utils

let append (xs : text_file pworkflow list) id =
  let echo_cmd = cmd "echo" [ string id ; string ">>" ; dest ] in
  Workflow.shell ~descr:"append" (match xs with
      | [] -> [ echo_cmd ]
      | _ :: _ -> [
          cmd "wc" ~stdout:dest [ string "-l" ; list ~sep:" " dep xs ] ;
          echo_cmd ;
        ])

let pipeline n debug =
  let root = append [] "root" in
  let l1 = List.init n ~f:(fun i -> append [ root ] (sprintf "l1_%d" i)) in
  let middle = append l1 "middle" in
  let l2 = List.init n ~f:(fun i -> append [ middle ] (sprintf "l2_%d" i)) in
  let final = append l2 "final" in
  List.map (final :: if debug then l2 else []) ~f:(fun x -> Workflow.eval_path x)
  |> Workflow.list

let main ~n ~debug ~dot_output () =
  let loggers = [
      Console_logger.create () ;
    ]
  in
  let w = pipeline n debug in
  if dot_output then Dot_output.workflow_to_file "accordion.dot" w ;
  ignore (Scheduler.simple_eval_exn ~loggers w : string list)

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Performance test on large pipelines"
    [%map_open
      let n = flag "-n" (required int) ~doc:"INT size of the pipeline"
      and debug = flag "--debug" no_arg ~doc:" sets many targets in the final repo"
      and dot_output = flag "--dot-output" no_arg ~doc:" produces a dot file" in
      main ~n ~debug ~dot_output
    ]

let () = Command.run command
