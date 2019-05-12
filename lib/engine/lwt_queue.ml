open Lwt.Infix
open Core_kernel

type 'a t = {
  queue : 'a Queue.t ;
  condition : unit Lwt_condition.t ;
}

let create () = {
  queue = Queue.create () ;
  condition = Lwt_condition.create () ;
}

let push q x =
  Queue.enqueue q.queue x ;
  Lwt_condition.signal q.condition ()

let rec pop q =
  match Queue.dequeue q.queue with
  | None ->
    Lwt_condition.wait q.condition >>= fun () ->
    pop q
  | Some x -> Lwt.return x
