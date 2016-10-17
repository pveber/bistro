open Core.Std
open Bistro_engine
open Lwt

type t = {
  queue : (Scheduler.time * Scheduler.event) Queue.t ;
  new_event : unit Lwt_condition.t ;
  loop : unit Lwt.t ;
}

let msg t fmt =
  let k s =
    let t = Time.(to_string (of_float t)) in
    printf "[%s] %s\n%!" t s
  in
  ksprintf k fmt

let rec loop queue new_event =
  match Queue.dequeue queue with
  | None ->
    Lwt_condition.wait new_event >>= fun () ->
    loop queue new_event
  | Some (t, ev) -> Task.(
      match ev with
      | Scheduler.Task_started (Step s) ->
        let id = String.prefix s.id 6 in
        msg t "started %s.%s" s.descr id

      | Scheduler.Task_ended (Step s, res) ->
        let id = String.prefix s.id 6 in
        let outcome = match res with
          | Ok () -> "(success)"
          | Error (`Msg msg) -> sprintf "error: %s" msg
        in
        msg t "ended %s.%s (%s)" s.descr id outcome

      | Scheduler.Task_skipped (Step s, `Allocation_error err) ->
        msg t "allocation error for %s.%s (%s)" s.descr s.id err

      | _ -> ()
    ) ;
    loop queue new_event


let create () =
  let queue = Queue.create () in
  let new_event = Lwt_condition.create () in
  let loop = loop queue new_event in
  { queue ; new_event ; loop }


let event log time event =
  Queue.enqueue log.queue (time, event) ;
  Lwt_condition.signal log.new_event ()
