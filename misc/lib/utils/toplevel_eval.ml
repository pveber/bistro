open Bistro
open Bistro_engine

module Make(P : sig
    val np : int
    val mem : int
  end)() =
struct
  open P

  let with_workflow w ~f =
    let open Scheduler in
    let db = Db.init_exn "_bistro" in
    let loggers = [ Console_logger.create () ] in
    let sched = create ~np:np ~mem:(`GB mem) ~loggers db in
    let thread = eval_exn sched w in
    start sched ;
    try
      Lwt_main.run thread
      |> f
    with
    | Failure msg as e -> (print_endline msg ; raise e)

  let eval w = with_workflow w ~f:(fun x -> x)

  let with_pworkflow w ~f = with_workflow (Workflow.path w) ~f

  let path w =
    with_pworkflow w ~f:(fun x -> x)

  let sh fmt =
    Printf.kprintf (fun s -> ignore (Sys.command s)) fmt

  let rm w = with_pworkflow w ~f:(sh "rm %s")

  let file w =
    with_pworkflow w ~f:(sh "file %s")

  let less w =
    with_pworkflow w ~f:(sh "less %s")

  let wc w =
    with_pworkflow w ~f:(sh "wc %s")

  let firefox w =
    with_pworkflow w ~f:(sh "firefox --no-remote %s")

  let evince w =
    with_pworkflow w ~f:(sh "evince %s")

  let ls w =
    with_pworkflow w ~f:(sh "ls %s")
end
