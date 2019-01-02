open Core
open Bistro_internals
open Bistro_engine

type time = float

type event =
  | Step_workflow_started : {
      id : string ;
      descr : string ;
    } -> event
  | Workflow_ended : Task_result.t -> event
  | Workflow_done_already : {
      w : _ Workflow.t ;
      path : string ;
    } -> event

type model = {
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
  model = { events = [] } ;
  stop = false ;
  changed = true ;
}

let translate_event db _ = function
  | Logger.Workflow_started (Shell {id ; descr ; _},  _) ->
    Some (Step_workflow_started { id ; descr })

  | Workflow_ended { outcome ; _ } ->
    Some (Workflow_ended outcome)

  | Workflow_skipped (w, `Done_already) ->
    let path = Db.cache db (Workflow.id w) in
    Some (Workflow_done_already { w ; path })

  | Workflow_ready _
  | Workflow_allocation_error _
  | Workflow_skipped (_, `Missing_dep)
  | Workflow_started (_, _)
  | Workflow_collected _
  | Singularity_image_collected _ -> None

let update model db time evt =
  {
    events = (
      match translate_event db time evt with
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

  let k = txt
  let kf fmt = Printf.ksprintf txt fmt

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
      let modals = List.map dumps ~f:(fun (Shell_command.File_dump { text ; path }) ->
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

  let shell_result_details ~id ~cmd ~cache ~stdout ~stderr ~file_dumps =
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
      div [
        item "command" [] ;
        pre [ k cmd ] ;
      ] ;
      step_file_dumps file_dumps ;
    ]

  let task_result = function
    | Task_result.Input { path ; pass ; _ } ->
      [ p [ k "input " ; k path ] ;
        if pass then k"" else (
          p [ k"Input doesn't exist" ]
        ) ]

    | Select { dir_path ; sel ; pass ; _ } ->
      [ p [ k "select " ;
            k (Bistro_engine.Path.to_string sel) ;
            k " in " ;
            k dir_path ] ;

        if pass then k"" else (
          p [ k"No path " ; k (Bistro_engine.Path.to_string sel) ; k" in " ;
              a ~a:[a_href dir_path] [k dir_path ] ]
        ) ]

    | Shell { exit_code ; outcome ; stdout ; stderr ; cache ; file_dumps ; cmd ; descr ; id } ->
      collapsible_panel
        ~title:[ k descr ]
        ~header:[
          match outcome with
          | `Succeeded -> k""
          | `Failed ->
            p [ kf "Command failed with code %d" exit_code ]
          | `Missing_output ->
            p [ k "Missing output" ]
        ]
        ~body:(shell_result_details ~id:id ~cmd ~cache ~stderr ~stdout ~file_dumps)
    | Plugin res ->
      collapsible_panel
        ~title:[ k res.descr ]
        ~header:[
          match res.outcome with
          | `Succeeded -> k""
          | `Failed ->
            p [ k (Option.value ~default:"" res.msg) ]
          | `Missing_output ->
            p [ k "missing_output" ]
        ]
        ~body:[]
    | Container_image_fetch f ->
      match f.outcome with
      | Ok () -> [ k"" ]
      | Error (`Singularity_failed_pull (_, url)) ->
        [ p [kf "Failed to fetch container image at %s" url] ]

  let workflow_start ~descr ~id =
    collapsible_panel
      ~title:[ k descr ]
      ~header:[]
      ~body:[
        item "id" [ k id ] ;
      ]

  let cached_workflow
    : type s. s Workflow.t -> string -> _
    = fun t path ->
    match t with
    | Workflow.Input { path ; _ } ->
      [ p [ k "input " ; k path ] ]
    | Select { dir = Input { path = input_path ; _ } ; sel ; _ } ->
      [ p [ k "select " ;
            k (Path.to_string sel) ;
            k " in " ;
            k input_path ] ]

    | Select { dir = Shell { id ; _ } ; sel ; _ } ->
      [ p [ k "select " ;
            k (Path.to_string sel) ;
            k " in step " ;
            k id ] ]

    | Select { dir = Select _ ; _} -> assert false
    | Shell { descr ; id ; _ } ->
      collapsible_panel
        ~title:[ k descr ]
        ~header:[]
        ~body:[ item "id" [ a ~a:[a_href path] [ k id ] ] ]
    | _ -> []

  let event_label_text col text =
    let col = match col with
      | `BLACK -> "black"
      | `RED -> "red"
      | `GREEN -> "green"
    in
    let style = sprintf "color:%s; font-weight:bold;" col in
    span ~a:[a_style style] [ k text ]

  let result_label = function
    | Task_result.Input { pass = true ; _ }
    | Select { pass = true ; _ } ->
      event_label_text `BLACK "CHECKED"

    | Input { pass = false ; _ }
    | Select { pass = false ; _ }
    | Plugin { outcome = `Failed ; _ }
    | Shell { outcome = `Failed ; _ } ->
      event_label_text `RED "FAILED"

    | Plugin { outcome = `Missing_output ; _ }
    | Shell { outcome = `Missing_output ; _ } ->
      event_label_text `RED "MISSING OUTPUT"

    | Shell { outcome = `Succeeded ; _ }
    | Plugin { outcome = `Succeeded ; _ }
    | Container_image_fetch { outcome = Ok () ; _ } ->
      event_label_text `GREEN "DONE"

    | Container_image_fetch { outcome = Error _ ; _ } ->
      event_label_text `RED "MISSING CONTAINER IMAGE"

  let event time evt =
    let table_line label details =
      [
        td [ k Time.(to_string_trimmed ~zone (of_tm ~zone (Unix.localtime time))) ] ;
        td [ label ] ;
        td details
      ]
    in
    match evt with
    | Step_workflow_started { id ; descr } ->
      table_line
        (event_label_text `BLACK "STARTED")
        (workflow_start ~id ~descr)

    | Workflow_ended result ->
      table_line (result_label result) (task_result result)

    | Workflow_done_already { w ; path } ->
      table_line (event_label_text `BLACK "CACHED") (cached_workflow w path)

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
    head (title (txt "Bistro report")) [
      link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
      link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
      script ~a:[a_src "https://code.jquery.com/jquery.js"] (txt "") ;
      script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (txt "") ;
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
    method event db time event =
      logger.model <- update logger.model db time event ;
      logger.changed <- true

    method stop =
      logger.stop <- true ;
      loop
  end

let create path = new logger path
