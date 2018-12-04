open Core
open Bistro

let append x y : text_file path workflow =
  Workflow.shell Shell_dsl.[
    cmd "cat" ~stdout:dest [ dep y ] ;
    cmd "echo" [ string x ; string ">>" ; dest ] ;
  ]

let%pworkflow start x : text_file path workflow =
  Out_channel.write_all [%dest] ~data:[%param x]

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

let pipeline =
  start "foo"
  |> append "bar"
  |> append "baz"
  |> append "gee"

let _ =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  Scheduler.eval_exn ~loggers:[logger] ~collect:true db pipeline
  |> Lwt_main.run
