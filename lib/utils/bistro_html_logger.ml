open Core.Std
open Bistro_engine

type time = float

type event =
  | Task_started of Task.t
  | Task_ended of Task.t * (unit, Task.error) result
  | Task_done_already of Task.t

type model = {
  dag : Scheduler.DAG.t option ;
  events : (time * event) list ;
}

type t = {
  path : string ;
  config : Task.config ;
  mutable model : model ;
  mutable queue : (Scheduler.time * Scheduler.event) list ;
  mutable stop : bool ;
  mutable loop : unit Lwt.t ;
}

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let create path config = {
  path ;
  config ;
  model = { dag = None ; events = [] } ;
  queue = [] ;
  stop = false ;
  loop = Lwt.return ()
}

let some_change logger = logger.queue <> []

let translate_event time = function
  | Scheduler.Task_started t ->
    Some (Task_started t)
  | Scheduler.Task_ended (t, outcome) ->
    Some (Task_ended (t, outcome))
  | Scheduler.Task_skipped (t, `Done_already) ->
    Some (Task_done_already t)

  | Scheduler.Init _
  | Scheduler.Task_ready _
  | Scheduler.Task_skipped (_, (`Allocation_error _ | `Missing_dep)) -> None

let update model time evt =
  {
    dag = (
      match evt with
      | Scheduler.Init dag -> Some dag
      | _ -> model.dag
    ) ;
    events = (
      match translate_event time evt with
      | None -> model.events
      | Some evt -> (time, evt) :: model.events
    ) ;
  }

module Render = struct
  open Tyxml_html
  let k = pcdata

  let task config = function
    | Task.Input (_, p) ->
      [ k "input " ; k (Bistro.string_of_path p) ]

    | Task.Select (_, `Input input_path, path) ->
      [ k "select " ;
        k (Bistro.string_of_path path) ;
        k " in " ;
        k (Bistro.string_of_path input_path) ]

    | Task.Select (_, `Step id, path) ->
      [ k "select " ;
        k (Bistro.string_of_path path) ;
        k " in step " ;
        k id ]

    | Task.Step  { Task.descr } ->
      [ details (summary [ k descr ]) [ k descr ] ]

  let event config time evt =
    let table_line action t =
      [
        td [ k (Float.to_string time) ] ;
        td [ action ] ;
        td (task config t)
      ]
    in
    match evt with
    | Task_started t ->
      table_line (k "STARTED") t

    | Task_ended (t, _) ->
      table_line (k "ENDED") t

    | Task_done_already t ->
      table_line (k "CACHED") t

  let model config m =
    [
      table (
        List.map m.events ~f:(fun (time, evt) -> tr (event config time evt))
      )
    ]

end

let render config model =
  let open Tyxml_html in
  let contents = Render.model config model in
  html (head (title (pcdata "")) []) (body contents)

let save path doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Lwt_io.with_file ~mode:Lwt_io.output path (fun oc ->
      let contents = Buffer.contents buf in
      Lwt_io.write oc contents
    )

let rec loop logger =
  if logger.stop then Lwt.return ()
  else if some_change logger then (
    logger.model <-
      List.fold_right logger.queue ~init:logger.model ~f:(fun (time, evt) model ->
          update model time evt
        ) ;
    logger.queue <- [] ;
    let doc = render logger.config logger.model in
    save logger.path doc >>= fun () ->
    loop logger
  )
  else (
    Lwt_unix.sleep 1. >>= fun () ->
    loop logger
  )

let start path config =
  let logger = create path config in
  logger.loop <- loop logger ;
  logger

let event logger time event =
  logger.queue <- (time, event) :: logger.queue

let stop logger =
  logger.stop <- true ;
  logger.loop
