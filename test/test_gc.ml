open Core
open Bistro

let append x y : text_file path workflow =
  Workflow.shell ~descr:(sprintf "append(%s)" x) Shell_dsl.[
    cmd "cat" ~stdout:dest [ dep y ] ;
    cmd "echo" [ string x ; string ">>" ; dest ] ;
  ]

let%pworkflow [@descr "start"]
    start x : text_file path workflow =
  Out_channel.write_all [%dest] ~data:[%param x]

let%workflow [@descr "explode"]
    explode x =
  In_channel.read_all [%path x]
  |> String.to_list

let%workflow
  [@descr "uppercase"]
    uppercase x =
  Char.uppercase [%eval x]

let%pworkflow [@descr "text_file_of_char_list"]
    text_file_of_char_list x : text_file path workflow =
  Out_channel.write_all [%dest] ~data:(String.of_char_list [%eval x])

let pipeline =
  start "foo"
  (* |> append "bar"
   * |> append "baz"
   * |> append "gee" *)
  |> explode
  |> Workflow.spawn ~f:uppercase
  |> text_file_of_char_list

let logger = object
  method event db _ : Bistro_engine.Logger.event -> unit = function
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
    | GC_registration gc_state ->
      Bistro_utils.Dot_output.to_file ~db ~gc_state "workflow.dot" pipeline ;
    | _ -> ()
  method stop = Lwt.return ()
end

let _ =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  Scheduler.eval_exn ~loggers:[logger] ~collect:true db pipeline
  |> Lwt_main.run
