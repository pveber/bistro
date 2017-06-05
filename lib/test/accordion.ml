open Core.Std
open Lwt
open Bistro.Std
open Bistro.EDSL
open Bistro_engine

module Workflow = Bistro.Workflow

type txt

let append (xs : txt workflow list) id =
  let echo_cmd = cmd "echo" [ string id ; string ">>" ; dest ] in
  workflow (match xs with
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
  let open Bistro_app in
  List.map (final :: if debug then l2 else []) ~f:(fun x -> pureW x)
  |> list

let main n debug dot_output () =
  let open Bistro_app in
  let logger =
    Bistro_logger.tee
      (if dot_output then Bistro_dot_output.create "accordion.dot" else Bistro_logger.null)
      (Bistro_console_logger.create ())
  in
  run ~logger (pipeline n debug)
  |> ignore

let command =
  Command.basic
    ~summary:"Performance test on large pipelines"
    Command.Spec.(
      empty
      +> flag "-n" (required int) ~doc:"INT size of the pipeline"
      +> flag "--debug" no_arg ~doc:" sets many targets in the final repo"
      +> flag "--dot-output" no_arg ~doc:" produces a dot file"
    )
    main
