open Core_kernel
open Bistro
open Bistro_nlp
open Bistro_utils

let cut_deps x = [%workflow
  let lines = In_channel.read_lines [%path x] in
  List.group lines ~break:(fun _ l -> l = "")
  |> List.filter ~f:(( <> ) [""])
]

let%pworkflow [@descr "dump_lines"] dump_lines x =
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

let logger = object
  method event _ _ : Bistro_engine.Logger.event -> unit = function
    | Workflow_started (w, _) ->
      printf "started %s\n%!" (Bistro_internals.Workflow.id w)
    | Workflow_ended { outcome ; _ } -> (
        match outcome with
        | Input { id ; _ }
        | Select { id ; _ }
        | Shell { id ; _ }
        | Other { id ; _ } -> printf "ended %s\n%!" id
      )
    | Workflow_collected w ->
      printf "collected %s\n%!" (Bistro_internals.Workflow.id w)
    | _ -> ()
  method stop = Lwt.return ()
end

let dump_gc_state sched fn =
  let open Bistro_engine in
  Option.iter (Scheduler.gc_state sched) ~f:(Bistro_utils.Dot_output.gc_state_to_file ~condensed:false fn)

let _ =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let pipeline = Repo.to_workflow repo ~outdir:"res" in
  Dot_output.workflow_to_file "workflow.dot" pipeline ;
  let sched = Scheduler.create ~np:4 ~loggers:[logger] ~collect:true db pipeline in
  ignore (Scheduler.run sched |> Lwt_main.run) ;
  dump_gc_state sched "gc_final.dot" ;
