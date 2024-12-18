open Core
open Bistro

let append x y : text file =
  Workflow.shell ~descr:(sprintf "append(%s)" x) Shell_dsl.[
    cmd "cat" ~stdout:dest [ dep y ] ;
    cmd "echo" [ string x ; string ">>" ; dest ] ;
  ]

let start x : text file =
  let f = fun%workflow dest ->
    Out_channel.write_all dest ~data:[%param x]
  in
  Workflow.path_plugin f

let explode x =
  let f = fun%workflow () ->
    In_channel.read_all [%path x]
    |> String.to_list
  in
  Workflow.plugin f

let uppercase x =
  let f = fun%workflow () ->
    Char.uppercase [%eval x]
  in
  Workflow.plugin f

let text_file_of_char_list x : text file =
  let f = fun%workflow dest ->
    Out_channel.write_all dest ~data:(String.of_char_list [%eval x])
  in
  Workflow.path_plugin f

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

let () =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let sched = Scheduler.create ~np:4 ~loggers:[Bistro_utils.Console_logger.create ()] ~collect:true db in
  let thread = Scheduler.eval_exn sched pipeline in
  Scheduler.start sched ;
  ignore (Lwt_main.run thread) ;
  dump_gc_state sched db "gc_final.dot"
