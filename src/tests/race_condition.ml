open Core.Std
open Printf

let rec task i deps_of_task : unit Bistro_workflow.t =
  Bistro_workflow.(
    make Shell_script.(
      begin_
        cmd "sleep 5"
        cmd "echo"  arg int i stdout_to dest
      end_
    )
    |> fun init ->
      List.fold_right deps_of_task
        ~init
        ~f:(fun dep accu -> depends ~on:dep accu)
  )


let log_event, send_to_log_event = React.E.create ()

let db = Bistro_db.init "_bistro"
let blog = Bistro_log.make ~db ~hook:(fun x -> send_to_log_event (Bistro_log.Entry.to_string x)) ()
let backend = Bistro_engine_lwt.local_worker ~np:4 ~mem:(6 * 1024) blog

(* let () = *)
(*   Lwt_unix.run (Bistro_concurrent.dryrun db (task 60)) *)

let blog_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream log_event)

let daemon = Bistro_engine_lwt.Daemon.make db blog backend

let t1 = task 1 []
let t2 = task 2 [ t1 ]
let t3 = task 3 [ t1 ]

let main () =
  let j2 = Option.value_exn (Bistro_engine_lwt.Daemon.send daemon t2) in
  let j3 = Option.value_exn (Bistro_engine_lwt.Daemon.send daemon t3) in
  Lwt.join [j2 ; j3]

let () = Lwt_unix.run (main ())

