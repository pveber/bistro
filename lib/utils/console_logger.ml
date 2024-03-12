open Core
open Lwt
open Bistro_engine

module Unix = Core_unix

let zone = Lazy.force Time_float_unix.Zone.local

let msg t fmt =
  let k s =
    let t = Time_float_unix.(to_string (of_tm ~zone (Unix.localtime t))) in
    printf "[%s] %s\n%!" t s
  in
  ksprintf k fmt

let error_short_descr_of_outcome = function
  | `Succeeded -> assert false
  | `Scheduler_error _
  | `Plugin_failure _ -> "failure"
  | `Error_exit_code exit_code ->
    sprintf "ended with exit code %d" exit_code
  | `Missing_output -> "missing output"

let error_short_descr =
  let open Execution_trace.Run_details in
  function
  | Input _ -> "input doesn't exist"
  | Select _ -> "invalid select"
  | Shell { outcome ; _ } -> error_short_descr_of_outcome outcome
  | Plugin { outcome ; _ } -> error_short_descr_of_outcome outcome
  | Container_image_fetch _ ->
    "failed to fetch container image"

let output_step_event t ~id ~descr =
  let id = String.prefix id 6 in
  msg t "started %s.%s" descr id

let output_event t = function
  | Logger.Workflow_started (Shell { id ; descr ; _ }, _) ->
    output_step_event t ~id ~descr
  | Logger.Workflow_started (Plugin { id ; descr ; _ }, _) ->
    output_step_event t ~id ~descr

  | Workflow_ended { details = (Execution_trace.Run_details.Shell { id ; descr ; _ } as outcome) ; _ } ->
    let id = String.prefix id 6 in
    let outcome_msg =
      if Execution_trace.Run_details.succeeded outcome then
        "success"
      else sprintf "error: %s" (error_short_descr outcome)
    in
    msg t "ended %s.%s (%s)" descr id outcome_msg

  | Workflow_ended { details = (Execution_trace.Run_details.Plugin { id ; descr ; _ } as outcome) ; _ } ->
    let id = String.prefix id 6 in
    let outcome_msg =
      if Execution_trace.Run_details.succeeded outcome then
        "success"
      else sprintf "error: %s" (error_short_descr outcome)
    in
    msg t "ended %s.%s (%s)" descr id outcome_msg

  | Logger.Workflow_allocation_error (Shell s, err) ->
    msg t "allocation error for %s.%s (%s)" s.descr s.id err

  | Logger.Workflow_allocation_error (Plugin s, err) ->
    msg t "allocation error for %s.%s (%s)" s.descr s.id err

  | Workflow_collected w ->
    msg t "collected %s" (Bistro_internals.Workflow.id w)

  | Debug m -> msg t "%s" m
  | _ -> ()

let rec loop stop queue new_event =
  match Queue.dequeue queue with
  | None ->
    if !stop then Lwt.return ()
    else
      Lwt_condition.wait new_event >>= fun () ->
      loop stop queue new_event

  | Some (t, ev) ->
    output_event t ev ;
    loop stop queue new_event

class t =
  let queue = Queue.create () in
  let new_event = Lwt_condition.create () in
  let stop = ref false in
  let loop = loop stop queue new_event in
  object
    method event (_ : Db.t) time event =
      Queue.enqueue queue (time, event) ;
      Lwt_condition.signal new_event ()

    method stop =
      stop := true ;
      Lwt_condition.signal new_event () ;
      loop
  end

let create () = new t
