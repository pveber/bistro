open Core.Std
open Bistro_engine

type time = float

type event =
  | Task_started of Task.t
  | Task_ended of Task.t * (unit, Task.error) result
  | Task_skipped of Task.t * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string]

type model = {
  dag : Scheduler.DAG.t option ;
  events : (time * event) list ;
}

type t = {
  path : string ;
  mutable model : model ;
  mutable queue : (Scheduler.time * Scheduler.event) list ;
  change : unit Lwt_condition.t ;
  stop : unit Lwt_mvar.t ;
  loop : unit Lwt.t ;
}

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let create path = {
  path ;
  model = { dag = None ; events = [] } ;
  queue = [] ;
  change = Lwt_condition.create () ;
  stop = Lwt_mvar.create () ;
  loop = Lwt.return ()
}

let handle_event = function
  | Scheduler.Task_started t ->
    Some (Task_started t)

  | _ -> None

let update model time = function
  | Scheduler.Init dag ->
    { model with dag = Some dag }
  | evt -> {
      model with events = (
      match handle_event evt with
      | None -> model.events
      | Some evt -> (time, evt) :: model.events
    )
    }

let render doc =
  let open Tyxml_html in
  html (head (title (pcdata "")) []) (body [])

let save path doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Lwt_io.with_file ~mode:Lwt_io.output path (fun oc ->
      let contents = Buffer.contents buf in
      Lwt_io.write oc contents
    )

let rec loop logger =
  logger.model <-
    List.fold_right logger.queue ~init:logger.model ~f:(fun (time, evt) model ->
        update model time evt
      ) ;
  logger.queue <- [] ;
  let doc = render logger.model in
  save logger.path doc >>= fun () ->
  Lwt.choose [
    Lwt_condition.wait logger.change >>| (fun () -> `change) ;
    Lwt_mvar.take logger.stop >>| (fun () -> `stop) ;
  ] >>=
  function
  | `change -> loop logger
  | `stop -> Lwt.return ()

let start path =
  let logger = create path in
  { logger with loop = loop logger }

let event logger time event =
  logger.queue <- (time, event) :: logger.queue ;
  Lwt_condition.signal logger.change ()

let stop logger =
  Lwt_mvar.put logger.stop () >>= fun () ->
  logger.loop
