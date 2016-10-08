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
  | Some (t, ev) ->
    Task.(
      match ev with
      | Scheduler.Task_started (Step s) ->
        msg t "started %s" s.descr
      | Scheduler.Task_ended (Step s, _) ->
        msg t "ended %s" s.descr
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
