open Core.Std
open Bistro_engine
open Lwt

let msg t fmt =
  let k s =
    let t = Time.(to_string (of_float t)) in
    printf "[%s] %s\n%!" t s ;
    Lwt.return ()
  in
  ksprintf k fmt

let error_short_descr =
  let open Task in
  function
  | Task.Input_doesn't_exist _ -> "input doesn't exist"
  | Task.Invalid_select _ -> "invalid select"
  | Task.Step_failure { exit_code } ->
    sprintf "ended with exit code %d" exit_code

let rec loop stop queue new_event =
  match Queue.dequeue queue with
  | None ->
    Lwt_condition.wait new_event >>= fun () ->
    if !stop then Lwt.return ()
    else loop stop queue new_event
  | Some (t, ev) ->
    Task.(
      match ev with
      | Scheduler.Task_started (Step s) ->
        let id = String.prefix s.id 6 in
        msg t "started %s.%s" s.descr id

      | Scheduler.Task_ended (Step s, res) ->
        let id = String.prefix s.id 6 in
        let outcome = match res with
          | Ok () -> "success"
          | Error e -> sprintf "error: %s" (error_short_descr e)
        in
        msg t "ended %s.%s (%s)" s.descr id outcome

      | Scheduler.Task_skipped (Step s, `Allocation_error err) ->
        msg t "allocation error for %s.%s (%s)" s.descr s.id err

      | _ -> Lwt.return ()
    ) >>= fun () ->
    loop stop queue new_event

class t =
  let queue = Queue.create () in
  let new_event = Lwt_condition.create () in
  let stop = ref false in
  let loop = loop stop queue new_event in
  object
    method event time event =
      Queue.enqueue queue (time, event) ;
      Lwt_condition.signal new_event ()

    method stop =
      stop := true

    method wait4shutdown = loop
  end

let create () = new t
