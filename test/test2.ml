open Core_kernel
open Bistro
open Bistro_nlp
open Bistro_utils

let cut_deps x = [%workflow
  let lines = In_channel.read_lines [%path x] in
  List.group lines ~break:(fun _ l -> String.equal l "")
  |> List.filter ~f:(Poly.( <> ) [""])
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
    )
  |> Repo.(items [ w ] ~prefix:"sentence" ~ext:"png")

let repo =
  [ "Protein" ; "Cell_(biology)" ]
  |> List.map ~f:pipeline

let dump_gc_state sched db fn =
  let open Bistro_engine in
  Option.iter (Scheduler.gc_state sched) ~f:(Bistro_utils.Dot_output.gc_state_to_file ~db ~condensed:false fn)

let () =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let pipeline = Repo.to_workflow repo ~outdir:"res" in
  Dot_output.workflow_to_file ~db "workflow.dot" pipeline ;
  let sched = Scheduler.create ~np:4 ~loggers:[Bistro_utils.Console_logger.create ()] ~collect:true db in
  let thread = Scheduler.eval_exn sched pipeline in
  Scheduler.start sched ;
  Lwt_main.run thread ;
  dump_gc_state sched db "gc_final.dot"
