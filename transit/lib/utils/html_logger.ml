open Core
open Bistro_engine

type time = float

type event =
  | Step_task_started of {
      step : Bistro.step ;
    }
  | Task_ended of Task.result
  | Task_done_already of {
      task : Task.t ;
      path : string ;
    }

type model = {
  dag : Scheduler.DAG.t option ;
  events : (time * event) list ;
}

let ( >>= ) = Lwt.( >>= )

let zone = Lazy.force Time.Zone.local

type t = {
  path : string ;
  mutable model : model ;
  mutable changed : bool ;
  mutable stop : bool ;
}

let create path = {
  path ;
  model = { dag = None ; events = [] } ;
  stop = false ;
  changed = true ;
}

let translate_event config _ = function
  | Scheduler.Task_started (Bistro.Step step, _) ->
    Some (Step_task_started { step })

  | Scheduler.Task_ended outcome ->
    Some (Task_ended outcome)

  | Scheduler.Task_skipped (task, `Done_already) ->
    let path = Db.cache config.Task.db (Task.id task) in
    Some (Task_done_already { task ; path })

  | Scheduler.Init _
  | Scheduler.Task_ready _
  | Scheduler.Task_skipped (_, (`Allocation_error _ | `Missing_dep))
  | Scheduler.Task_started ((Bistro.Input _ | Bistro.Select _), _) -> None

let update model config time evt =
  {
    dag = (
      match evt with
      | Scheduler.Init { dag ; _ } -> Some dag
      | _ -> model.dag
    ) ;
    events = (
      match translate_event config time evt with
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

  let collapsible_panel ~title ~header ~body =
    let elt_id = new_elt_id () in
    [
      div ~a:[a_class ["panel-group"]] [
        div ~a:[a_class ["panel";"panel-default"]] [
          div ~a:[a_class ["panel-heading"]] (
            h4 ~a:[a_class ["panel-title"]] [
              a ~a:[a_user_data "toggle" "collapse" ; a_href ("#" ^ elt_id) ] title
            ]
            :: header
            @ [ div ~a:[a_id elt_id ; a_class ["panel-collapse";"collapse"]] body ]
          )
        ]
      ]
    ]


  let modal ~header ~body =
    let modal_id = new_elt_id () in
    let elt =
      div ~a:[a_id modal_id ; a_class ["modal";"fade"] ; Unsafe.string_attrib "role" "dialog"] [
        div ~a:[a_class ["modal-dialog"] ; a_style "width:70%"] [
          div ~a:[a_class ["modal-content"]] [
            div ~a:[a_class ["modal-header"]] [
              button ~a:[a_class ["close"] ; a_user_data "dismiss" "modal"] [
                entity "times"
              ] ;
              h4 header
            ] ;
            div ~a:[a_class ["modal-body"]] body
          ]
        ]
      ]
    in
    modal_id, elt

  let item label contents =
    p (strong [k (label ^ ": ")] :: contents)


  let step_file_dumps = function
    | [] -> k"" ;
    | _ :: _ as dumps ->
      let modals = List.map dumps ~f:(fun (Task.File_dump { text ; path }) ->
          let id, modal =
            modal ~header:[ k path ] ~body:[ pre [ k text ] ]
          in
          id, path, modal
        )
      in
      let links =
        List.map modals ~f:(fun (modal_id, fn, _) ->
            li [
              button ~a:[
                a_class ["btn-link"] ;
                a_user_data "toggle" "modal" ;
                a_user_data "target" ("#" ^ modal_id)] [ k fn ]
            ]
          )
        |> ul
      in
      div (
        (item "file dumps" [])
        :: links
        :: List.map modals ~f:(fun (_,_,x) -> x)
      )

  let step_action = function
    | `Sh cmd -> div [
        item "command" [] ;
        pre [ k cmd ] ;
      ]
    | `Eval -> div []

  let step_result_details ~id ~action ~cache ~stdout ~stderr ~file_dumps =
    let outputs = div [
        item "outcome" [
          a ~a:[a_href stdout ] [ k "stdout" ] ;
          k " " ;
          a ~a:[a_href stderr ] [ k "stderr" ] ;
        ] ;
      ]
    in
    [
      item "id" [
        match cache with
        | Some file_uri ->
          a ~a:[a_href file_uri] [ k id ]
        | None -> k id
      ] ;

      outputs ;
      step_action action ;
      step_file_dumps file_dumps ;
    ]

  let task_result =
    let open Task in
    function
    | Input_check { path ; pass } ->
      [ p [ k "input " ; k path ] ;
        if pass then k"" else (
          p [ k"Input doesn't exist" ]
        ) ]

    | Select_check { dir_path ; sel ; pass } ->
      [ p [ k "select " ;
            k (Bistro.Path.to_string sel) ;
            k " in " ;
            k dir_path ] ;

        if pass then k"" else (
          p [ k"No path " ; k (Bistro.Path.to_string sel) ; k" in " ;
              a ~a:[a_href dir_path] [k dir_path ] ]
        ) ]

    | Step_result { exit_code ; outcome ; step ; stdout ; stderr ; cache ; file_dumps ; action } ->
      collapsible_panel
        ~title:[ k step.descr ]
        ~header:[
          match outcome with
          | `Succeeded -> k""
          | `Failed ->
            p [ k (sprintf "Command failed with code %d" exit_code) ]
          | `Missing_output ->
            p [ k "missing_output" ]
        ]
        ~body:(step_result_details ~id:step.id ~action ~cache ~stderr ~stdout ~file_dumps)
    | Map_command_result _ -> assert false (* FIXME *)

  let task =
    let open Bistro in
    function
    | Input (_, path) ->
      [ p [ k "input " ; k (Path.to_string path) ] ]

    | Select (_, Input (_, input_path), path) ->
      [ p [ k "select " ;
            k (Path.to_string path) ;
            k " in " ;
            k (Path.to_string input_path) ] ]

    | Select (_, Step { id ; _ }, path) ->
      [ p [ k "select " ;
            k (Path.to_string path) ;
            k " in step " ;
            k id ] ]

    | Select (_, Select _, _) -> assert false

    | Step { descr ; id ; _ } ->
      collapsible_panel
        ~title:[ k descr ]
        ~header:[]
        ~body:[ item "id" [ k id ] ]

  let task_start ~step:{ Bistro.descr ; id ; _ } =
    collapsible_panel
      ~title:[ k descr ]
      ~header:[]
      ~body:[
        item "id" [ k id ] ;
      ]

  let cached_task t path =
    let open Bistro in
    match t with
    | Input _
    | Select _ -> task t
    | Step { descr ; id ; _ } ->
      collapsible_panel
        ~title:[ k descr ]
        ~header:[]
        ~body:[ item "id" [ a ~a:[a_href path] [ k id ] ] ]

  let event_label_text col text =
    let col = match col with
      | `BLACK -> "black"
      | `RED -> "red"
      | `GREEN -> "green"
    in
    let style = sprintf "color:%s; font-weight:bold;" col in
    span ~a:[a_style style] [ k text ]

  let result_label =
    let open Task in
    function
    | Input_check { pass = true ; _ }
    | Select_check { pass = true ; _ } ->
      event_label_text `BLACK "CHECKED"

    | Input_check { pass = false ; _ }
    | Select_check { pass = false ; _ }
    | Step_result { outcome = `Failed ; _ } ->
      event_label_text `RED "FAILED"

    | Step_result { outcome = `Missing_output ; _ } ->
      event_label_text `GREEN "MISSING OUTPUT"

    | Step_result { outcome = `Succeeded ; _ } ->
      event_label_text `GREEN "DONE"
    | Map_command_result _ -> assert false (* FIXME *)

  let event time evt =
    let table_line label details =
      [
        td [ k Time.(to_string_trimmed ~zone (of_tm ~zone (Unix.localtime time))) ] ;
        td [ label ] ;
        td details
      ]
    in
    match evt with
    | Step_task_started { step } ->
      table_line
        (event_label_text `BLACK "STARTED")
        (task_start ~step)

    | Task_ended result ->
      table_line (result_label result) (task_result result)

    | Task_done_already { task = t ; path } ->
      table_line (event_label_text `BLACK "CACHED") (cached_task t path)

  let event_table m =
    let table =
      List.map m.events ~f:(fun (time, evt) -> tr (event time evt))
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

  let model m =
    let contents = List.concat [
        event_table m ;
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
  if logger.changed then (
    let doc = Render.model logger.model in
    logger.changed <- false ;
    save logger.path doc >>= fun () ->
    loop logger
  )
  else if logger.stop then Lwt.return ()
  else (
    Lwt_unix.sleep 1. >>= fun () ->
    loop logger
  )

class logger path =
  let logger = create path in
  let loop = loop logger in
  object
    method event config time event =
      logger.model <- update logger.model config time event ;
      logger.changed <- true

    method stop =
      logger.stop <- true

    method wait4shutdown = loop
  end

let create path = new logger path
