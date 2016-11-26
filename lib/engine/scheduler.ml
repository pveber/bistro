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

module DAG = struct
  include Tdag.Make(Domain)

  let dot_output dag fn =
    let vertex_attribute =
      let open Task in
      function
      | Input (_, p) ->
        let label = Bistro.string_of_path p in
        [ `Label label ; `Color 0xFFFFFF ; `Shape `Box ]
      | Select (_, _, p) ->
        let label = Bistro.string_of_path p in
        [ `Label label ; `Color 0xFFFFFF ; `Shape `Box ]
      | Step { descr } ->
        [ `Label descr ; `Shape `Box ]
    in
    let edge_attribute =
      let open Task in
      function
      | Select _, Step _ -> [ `Style `Dotted ]
      | _ -> []
    in
    dot_output dag vertex_attribute edge_attribute fn
end

include DAG

let workflow_deps =
  let open Bistro in
  function
  | Input _ -> []
  | Select (_, dir, _, _) -> [ dir ]
  | Step s -> s.deps

let workflow_id =
  let open Bistro in
  function
  | Input (id, _, _)
  | Select (id, _, _, _)
  | Step { id } -> id

let rec add_workflow dag w =
  let u = Task.of_workflow w in
  let dag' =
    List.fold (workflow_deps w) ~init:(DAG.add_task dag u) ~f:(fun accu dep ->
        let accu', maybe_v = add_workflow accu dep in
        match maybe_v with
        | None -> accu'
        | Some v -> DAG.add_dep accu' u ~on:v
      )
  in
  dag', Some u

let compile workflows =
  List.fold workflows ~init:DAG.empty ~f:(fun accu (Bistro.Workflow w) ->
      add_workflow accu (Bistro.Workflow.u w)
      |> fst
    )

let run ?logger alloc config dag =
  DAG.run ?logger alloc config dag
