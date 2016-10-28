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

  let new_elt_id =
    let c = ref 0 in
    fun () ->
      incr c ;
      sprintf "id%08d" !c

  let k = pcdata

  let step_details config ({Task.id ; np ; mem } as step)  =
    let file_uri = Db.cache config.Task.db id in
    [
      p [ strong [k"id: "] ; a ~a:[a_href file_uri] [ k id ]] ;
      p [ strong [k"command:"] ] ;
      pre [ k (Task.render_step_command ~np ~mem config step) ] ;
    ]

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

    | Task.Step step ->
      let elt_id = new_elt_id () in
      [
        div ~a:[a_class ["panel-group"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-heading"]] [
              h4 ~a:[a_class ["panel-title"]] [
                a ~a:[a_user_data "toggle" "collapse" ; a_href ("#" ^ elt_id) ] [ k step.Task.descr ]
              ]
            ] ;
            div ~a:[a_id elt_id ; a_class ["panel-collapse";"collapse"]] (
              step_details config step ;
            )

          ]
        ]
      ]

  let event config time evt =
    let table_line action t =
      [
        td [ k Time.(to_string_trimmed ~zone:Zone.local (of_float time)) ] ;
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

  let event_table config m =
    let table =
      List.map m.events ~f:(fun (time, evt) -> tr (event config time evt))
      |> table ~a:[a_class ["table"]]
    in
    [
      p ~a:[a_style {|font-weight:700; color:"#959595"|}] [ k"EVENT LOG" ] ;
      table ;
    ]

  let head =
    head (title (pcdata "Bistro report")) [
      link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
      link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
      script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
      script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
    ]

  let model config m =
    let contents = List.concat [
        event_table config m ;
      ]
    in
    html head (body [ div ~a:[a_class ["container"]] contents ])

end

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
    let doc = Render.model logger.config logger.model in
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
