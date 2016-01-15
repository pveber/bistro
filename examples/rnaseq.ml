open Core.Std
open Bistro_bioinfo.Std




open Bistro_engine
open Lwt

let db = Db.init_exn "_bistro"
let scheduler = Scheduler.(make (local_backend ~np:4 ~mem:(10 * 1024)) db)


let main goal =
  Scheduler.build scheduler goal >|= function
  | `Ok p -> print_endline p
  | `Error xs ->
    fprintf stderr "Some workflow(s) failed:\n" ;
    List.iter xs ~f:(fun (u, msg) ->
        fprintf stderr "\t%s\t%s\n" (Bistro.Workflow.id' u) msg
      ) ;
    List.iter xs ~f:(fun (u, _) -> Db.output_report db u stderr)

(* let () = Lwt_unix.run (main ()) *)
