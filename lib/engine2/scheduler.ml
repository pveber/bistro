open Core_kernel.Std

module Domain = struct
  module Thread = Lwt
  module Allocator = Allocator
  module Task = Task
end

module DAG = Tdag.Make(Domain)

let rec add_workflow dag =
  function
  | Bistro.Input _ -> dag, None (* FIXME: should test if input is present! *)
  | Bistro.Select (_, dir, _) -> add_workflow dag dir
  | Bistro.Step s ->
    let u = Task.of_step s in
    let dag' =
      List.fold s.Bistro.deps ~init:dag ~f:(fun accu dep ->
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

