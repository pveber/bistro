open Core_kernel.Std
open Tdag_sig

module Make(D : Domain) = struct
  open D

  let ( >>= ) = Thread.bind
  let ( >>| ) x f = x >>= fun x -> Thread.return (f x)

  let rec map_p ~f = function
    | [] -> Thread.return []
    | h :: t ->
      let f_h = f h and map_f_t = map_p t ~f in
      f_h >>= fun f_h ->
      map_f_t >>| fun map_f_t ->
      f_h :: map_f_t

  module V = struct
    type t = Task.t
    let compare u v = String.compare (Task.id u) (Task.id v)
    let hash u = Hashtbl.hash (Task.id u)
    let equal u v =
      Task.id u = Task.id v
  end

  module G = Graph.Persistent.Digraph.Concrete(V)
  module Dfs = Graph.Traverse.Dfs(G)

  type t = G.t
  type task = Task.t
  type 'a thread = 'a Thread.t
  type allocator = Allocator.t
  type config = Task.config
  type event =
    | Task_ready of task
    | Task_started of task
    | Task_ended of task * unit result

  let empty = G.empty

  let add_task = G.add_vertex

  let add_dep g u ~on:v =
    G.add_edge g u v

  let sources g =
    let f u accu =
      if G.in_degree g u = 0 then u :: accu
      else accu
    in
    G.fold_vertex f g []

  let successfull_trace = function
    | Run { outcome = Ok () }
    | Skipped `Done_already -> true
    | _ -> false

  let rec dft log alloc config g thread_table u =
    let id = Task.id u in
    if String.Map.mem thread_table id then
      thread_table
    else
      let thread_table = G.fold_succ (Fn.flip (dft log alloc config g)) g u thread_table in
      if String.Map.mem thread_table id then
        thread_table
      else
        let foreach_succ v accu =
          String.Map.find_exn thread_table (Task.id v) :: accu
        in
        let thread =
          Task.is_done config u >>= fun is_done ->
          if is_done then
            Thread.return (Skipped `Done_already)
          else
            map_p ~f:ident (G.fold_succ foreach_succ g u []) >>= fun dep_traces ->
            if List.for_all dep_traces ~f:successfull_trace then (
              let ready = Unix.gettimeofday () in
              log ready (Task_ready u) ;
              Allocator.request alloc (Task.requirement u) >>= fun resource ->
              let start = Unix.gettimeofday () in
              log start (Task_started u) ;
              Task.perform resource config u >>= fun outcome ->
              let end_ = Unix.gettimeofday () in
              log end_ (Task_ended (u, outcome)) ;
              Allocator.release alloc resource ;
              Thread.return (Run { ready ; start ; end_ ; outcome })
            )
            else
              Thread.return (Skipped `Missing_dep)
        in
        String.Map.add thread_table id thread

  let run ?(log = fun _ _ -> ()) config alloc g =
    if Dfs.has_cycle g then failwith "Cycle in dependency graph" ;
    let sources = sources g in
    let ids, threads =
      List.fold sources ~init:String.Map.empty ~f:(dft log alloc config g)
      |> String.Map.to_alist
      |> List.unzip
    in
    map_p threads ~f:ident >>| fun traces ->
    List.zip_exn ids traces
    |> String.Map.of_alist_exn
end
