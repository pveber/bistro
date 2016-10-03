open Core_kernel.Std
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

module Task = struct
  type t = Push of int
  type request = Even | Odd
  type resource = request
  type 'a thread = 'a Lwt.t

  let id (Push i) = string_of_int i

  let requirement (Push i) =
    if i mod 0 = 0 then Even
    else Odd

  let perform _ (Push i) =
    log (Started i) ;
    Lwt_unix.sleep (Random.float 0.1) >>| fun () ->
    log (Ended i) ;
    performed := i :: !performed ;
    Ok ()

  let clean _ = Lwt.return ()

  let is_done (Push i) =
    Lwt.return (List.mem !performed i)
end

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
      Lwt.return a.value
    )
    else (
      Lwt_condition.wait a.cond >>= fun () ->
      request a
    )

  let free a =
    a.state <- true ;
    Lwt_condition.signal a.cond ()
end

module Allocator = struct
  type resource = Task.resource
  type request = resource
  type 'a thread = 'a Lwt.t

  type t = {
    even : resource Token_allocator.t ;
    odd : resource Token_allocator.t ;
  }

  let create () = {
    even = Token_allocator.create Task.Even ;
    odd = Token_allocator.create Task.Odd ;
  }

  let request alloc = function
    | Task.Odd -> Token_allocator.request alloc.odd
    | Task.Even -> Token_allocator.request alloc.even

  let free alloc = function
    | Task.Odd -> Token_allocator.free alloc.odd
    | Task.Even -> Token_allocator.free alloc.even
end

module TG = struct
  include Tdag.Make(Task)(Allocator)(Lwt)

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
end

let () =
  Lwt_unix.run (TG.run (make 30))
