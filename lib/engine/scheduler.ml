open Core_kernel.Std
open Bistro_tdag

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


(* If [w] is a select, we need to ensure its parent dir is
   marked as precious. [w] only performs a side effect, the
   real contents is in the result of selected workflow. *)
let precious_expand =
  let open Bistro in
  function
  | (Input _ as w) -> [ w ]
  | Select (_, (Step _ as s), _) as w ->
    [ w ; s ]
  | Select (_, (Input _ | Select _), _) as w -> [ w ]
  | Step _ as w -> [ w ]

let precious_expansion = List.concat_map ~f:precious_expand


let rec add_workflow precious_set (seen, dag) w =
  let id = workflow_id w in
  match String.Map.find seen id with
  | None ->
    let precious = String.Set.mem precious_set id in
    let u = Task.of_workflow ~precious w in
    let seen', dag' =
      List.fold (workflow_deps w) ~init:(seen, DAG.add_task dag u) ~f:(fun accu dep ->
          (* If [dep] is a select, we need to add its parent dir as a
             dep of [u], because [dep] only performs a side effect,
             the real contents that [u] needs is in the result of
             [dep] parent directory.*)
          let accu = Bistro.(
              match dep with
              | Select (_, dep_dir, _) ->
                let seen, dag, dep_dir_v = add_workflow precious_set accu dep_dir in
                seen, DAG.add_dep dag u ~on:dep_dir_v
              | Input _ | Step _ -> accu
            )
          in
          let seen, dag, dep_v = add_workflow precious_set accu dep in
          String.Map.add seen id u,
          DAG.add_dep dag u ~on:dep_v
        )
    in
    seen', dag', u

  | Some u -> seen, dag, u


let compile workflows =
  let workflows =
    List.map workflows ~f:(fun (Bistro.Workflow w) -> Bistro.Workflow.u w)
  in
  let precious_ids =
    workflows
    |> precious_expansion
    |> List.map ~f:Bistro.Workflow.id'
    |> String.Set.of_list
  in
  let _, dag, goals =
    List.fold workflows ~init:(String.Map.empty, DAG.empty, []) ~f:(fun (seen, dag, goals) u ->
        let seen, dag, t = add_workflow precious_ids (seen, dag) u in
        seen, dag, t :: goals
    )
  in
  dag, goals

let run ?logger ?goals alloc config dag =
  DAG.run ?logger ?goals alloc config dag
