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
  | Select (_, dir, _) -> [ dir ]
  | Step s -> s.deps

let workflow_id =
  let open Bistro in
  function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id

let rec add_workflow (seen, dag) w =
  let id = workflow_id w in
  match String.Map.find seen id with
  | None ->
    let u = Task.of_workflow w in
    let seen', dag' =
      List.fold (workflow_deps w) ~init:(seen, DAG.add_task dag u) ~f:(fun accu dep ->
          (* If [dep] is a select, we need to add its parent dir as a
             dep of [u], because [dep] only performs a side effect,
             the real contents that [u] needs is in the result of
             [dep] parent directory.*)
          let accu = Bistro.(
              match dep with
              | Select (_, dep_dir, _) ->
                let seen, dag, dep_dir_v = add_workflow accu dep_dir in
                seen, DAG.add_dep dag u ~on:dep_dir_v
              | Input _ | Step _ -> accu
            )
          in
          let seen, dag, dep_v = add_workflow accu dep in
          String.Map.add seen id u,
          DAG.add_dep dag u ~on:dep_v
        )
    in
    seen', dag', u

  | Some u -> seen, dag, u

let compile workflows =
  workflows
  |> List.map ~f:(fun (Bistro.Workflow w) -> Bistro.Workflow.u w)
  |> Bistro.Workflow.precious_propagation
  |> List.fold ~init:(String.Map.empty, DAG.empty, []) ~f:(fun (seen, dag, goals) u ->
      let seen, dag, t = add_workflow (seen, dag) u in
      seen, dag, t :: goals
    )
  |> fun (_, dag, goals) -> dag, goals

let run ?logger ?goals alloc config dag =
  DAG.run ?logger ?goals alloc config dag
