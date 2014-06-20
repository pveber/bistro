open Core.Std
open Printf

let ( >>= ) = Lwt.bind

let rec task i =
  let task_deps =
    List.init (i - 1) succ
    |> List.filter_map ~f:(
      fun j ->
        if j <> 1 && i mod j = 0 then Some (task j)
        else None
    )
  in
  Bistro_workflow.(
    make Shell_script.(
        begin_
          cmd "sleep" arg int 4
          cmd "echo"  arg int i stdout_to dest
        end_
    )
    |> fun init ->
      List.fold_right task_deps
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

let rec main () =
  Lwt_io.read_line Lwt_io.stdin >>= fun l ->
  ignore (Bistro_engine_lwt.Daemon.run daemon (task (Int.of_string l))) ;
  main ()

let () =
  Lwt_unix.run (main ())
