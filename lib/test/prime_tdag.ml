open Core.Std
open Rresult

let divides i j =
  j mod i = 0

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

type event =
  | Started of int
  | Ended of int

let events : event list ref = ref []
let performed : int list ref = ref []

let check_state set =
  Int.Set.iter set ~f:(fun i ->
      Int.Set.iter set ~f:(fun j ->
          if i <> j && (i mod 2 = j mod 2)
          then failwith "Violated resource constraint" ;
          if i <> j && (divides i j || divides j i)
          then failwith "Violated dependency"
        )
    )

let check_events xs =
  List.fold xs ~init:Int.Set.empty ~f:(fun accu ev ->
      let accu = match ev with
        | Started i -> Int.Set.add accu i
        | Ended i -> Int.Set.remove accu i
      in
      check_state accu ;
      accu
    )
  |> ignore

let check_performed xs =
  if List.contains_dup xs
  then failwith "A task has been performed twice"

let log e =
  events := e :: !events

let started i =
  log (Started i)

let ended i =
  log (Ended i) ;
  performed := i :: !performed


module Token_allocator = struct
  type 'a t = {
    value : 'a ;
    mutable state : bool ;
    cond : unit Lwt_condition.t ;
  }

  let create value = {
    value ;
    state = true ;
    cond = Lwt_condition.create () ;
  }

  let rec request a =
    if a.state then (
      a.state <- false ;
      Lwt.return (Ok a.value)
    )
    else (
      Lwt_condition.wait a.cond >>= fun () ->
      request a
    )

  let release a =
    a.state <- true ;
    Lwt_condition.signal a.cond ()
end

module Allocator = struct
  type request = Even | Odd
  type resource = request

  type t = {
    even : resource Token_allocator.t ;
    odd : resource Token_allocator.t ;
  }

  let create () = {
    even = Token_allocator.create Even ;
    odd = Token_allocator.create Odd ;
  }

  let request alloc = function
    | Odd -> Token_allocator.request alloc.odd
    | Even -> Token_allocator.request alloc.even

  let release alloc = function
    | Odd -> Token_allocator.release alloc.odd
    | Even -> Token_allocator.release alloc.even
end

module Task = struct
  type t = Push of int
  type config = unit
  type result = t * (unit, [ `Msg of string ]) Pervasives.result

  let id (Push i) = string_of_int i

  let requirement (Push i) =
    if i mod 2 = 0 then Allocator.Even
    else Allocator.Odd

  let perform _ _ (Push i as t) =
    log (Started i) ;
    Lwt_unix.sleep (Random.float 0.5) >>| fun () ->
    log (Ended i) ;
    performed := i :: !performed ;
    t, Ok ()

  let clean _ _ = Lwt.return ()

  let is_done _ (Push i) =
    Lwt.return (List.mem !performed i)

  let failure (_, x) = x = Ok ()
end

module D = struct
  module Thread = Lwt
  module Allocator = Allocator
  module Task = Task
end

module TG = struct
  include Tdag.Make(D)

  let make n =
    let module S = Sequence in
    let add_task accu i =
      let init = add_task accu (Task.Push i) in
      S.range 2 (i - 1)
      |> S.fold ~init ~f:(fun accu j ->
          if divides j i then
            add_dep accu (Task.Push i) ~on:(Task.Push j)
          else
            accu
        )
    in
    S.range 2 n
    |> S.fold ~init:empty ~f:add_task

  let logger = object
    method event _ t evt =
      let t = Time.to_string (Time.of_float t) in
      match evt with
      | Task_ready (Task.Push i) -> printf "[%s] ready push %d\n%!" t i
      | Task_started (Task.Push i) -> printf "[%s] started push %d\n%!" t i
      | Task_ended (Task.Push i, _) -> printf "[%s] ended push %d\n%!" t i
      | _ -> ()
    method stop = ()
    method wait4shutdown = Lwt.return ()
  end
end

let command =
  Command.basic
    ~summary:"Tests job scheduling on an integer divisor DAG"
    Command.Spec.empty
    (fun () ->
       Lwt_unix.run (
         TG.run ~logger:TG.logger () (Allocator.create ()) (TG.make 100) >>| fun _ ->
         check_performed (List.rev !performed) ;
         check_events (List.rev !events)
       ))
