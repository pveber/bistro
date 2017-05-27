open Core

type state =
  | Init
  | Dumping of unit Lwt.t

type t = {
  path : string ;
  mutable state : state ;
}

let start path = {
  path ;
  state = Init ;
}

let event logger _ ev =
  match logger.state, ev with
  | Init, Scheduler.Init dag ->
    logger.state <- Dumping (
        Lwt_preemptive.detach (fun () ->
            let tmp = Filename.temp_file "bistro_app" ".dot" in
            Scheduler.DAG.dot_output dag tmp ;
            Sys.command (sprintf "dot -Tpdf %s > %s" tmp logger.path) |> ignore
          ) ()
      )

  | _ -> ()

let stop logger = match logger.state with
  | Init -> Lwt.return ()
  | Dumping t -> t

