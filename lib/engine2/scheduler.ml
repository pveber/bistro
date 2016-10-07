open Core_kernel.Std

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )
let ( >>=? ) x f = x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

module Domain = struct
  module Thread = Lwt
  module Allocator = Allocator
  module Task = Task
end

module DAG = Tdag.Make(Domain)

let workflow_deps =
  let open Bistro in
  function
  | Input _ -> []
  | Select (_, dir, _) -> [ dir ]
  | Step s -> s.deps

let workflow_id =
  let open Bistro in
  function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id

let rec add_workflow dag w =
  let u = Task.of_workflow w in
  let dag' =
    List.fold (workflow_deps w) ~init:dag ~f:(fun accu dep ->
        let accu', maybe_v = add_workflow accu dep in
        match maybe_v with
        | None -> accu'
        | Some v -> DAG.add_dep accu' u ~on:v
      )
  in
  dag', Some u


let run alloc config workflows =
  let dag = List.fold workflows ~init:DAG.empty ~f:(fun accu (Bistro.Workflow w) ->
      add_workflow accu (Bistro.u w)
      |> fst
    )
  in
  DAG.run alloc config dag
