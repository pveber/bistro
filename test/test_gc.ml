open Core
open Bistro

let append x y : text_file path workflow =
  Workflow.shell ~descr:(sprintf "append(%s)" x) Shell_dsl.[
    cmd "cat" ~stdout:dest [ dep y ] ;
    cmd "echo" [ string x ; string ">>" ; dest ] ;
  ]

let%pworkflow start x : text_file path workflow =
  Out_channel.write_all [%dest] ~data:[%param x]

let%workflow explode x =
  In_channel.read_all [%path x]
  |> String.to_list

let%workflow uppercase x =
  Char.uppercase [%eval x]

let%pworkflow text_file_of_char_list x : text_file path workflow =
  Out_channel.write_all [%dest] ~data:(String.of_char_list [%eval x])

let pipeline =
  start "foo"
  |> append "bar"
  |> append "gee"
  |> explode
  |> Workflow.spawn ~f:uppercase
  |> text_file_of_char_list

let dump_gc_state sched db fn =
  let open Bistro_engine in
  Option.iter (Scheduler.gc_state sched) ~f:(Bistro_utils.Dot_output.gc_state_to_file ~db ~condensed:false fn)

let _ =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let sched = Scheduler.create ~np:4 ~loggers:[Bistro_utils.Console_logger.create ()] ~collect:true db in
  let thread = Scheduler.eval_exn sched pipeline in
  Scheduler.start sched ;
  ignore (Lwt_main.run thread) ;
  dump_gc_state sched db "gc_final.dot"
