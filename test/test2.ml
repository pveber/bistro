open Core_kernel
open Lwt.Infix
open Bistro
open Bistro_nlp

let cut_deps x = [%workflow
  let lines = In_channel.read_lines [%path x] in
  List.group lines ~break:(fun _ l -> l = "")
]

let%pworkflow dump_lines x =
  Out_channel.write_lines [%dest] [%eval x]

let pipeline w =
  wikipedia_summary w
  |> Stanford_parser.lexparser
  |> cut_deps
  |> Workflow.spawn ~f:(fun deps ->
      dump_lines deps
      |> Stanford_parser.dependensee
      |> Workflow.eval_path
    )

let _ =
  let open Bistro_engine in
  let config = { Scheduler.db = Db.init_exn "_bistro" ; use_docker = true } in
  Lwt_main.run (
    Bistro_engine.Scheduler.eval config (pipeline "Protein") >|= fun files ->
    Printf.printf "%s\n" (String.concat ~sep:"\n" files)
  )
