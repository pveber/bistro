open Core.Std
open Bistro_engine

type time = float

type model = {
  dag : Scheduler.DAG.t option ;
  events : (time * Scheduler.event) list ;
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

let update model time = function
  | Scheduler.Init dag ->
    { model with dag = Some dag }
  | evt -> {
      model with events = (time, evt) :: model.events
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

  let selected_event config time evt_type t =
    [
      td [ k (Float.to_string time) ] ;
      td [ evt_type ] ;
      td (task config t)
    ]

  let event config time evt =
    let open Scheduler in
    match evt with
    | Task_started t ->
      Some (selected_event config time (k "STARTED") t)
    | Task_ended (t, _) ->
      Some (selected_event config time (k "ENDED") t)
    | Task_skipped (t, `Done_already) ->
      Some (selected_event config time (k "CACHED") t)

    | Init _
    | Task_ready _
    | Task_skipped (_, (`Allocation_error _ | `Missing_dep)) -> None

  let model config m =
    [
      table (
        List.filter_map m.events ~f:(fun (time, evt) -> event config time evt)
        |> List.map ~f:tr
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
