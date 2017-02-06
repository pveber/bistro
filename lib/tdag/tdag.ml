open Core_kernel.Std
open Tdag_sig

module Make(D : Domain) = struct
  open D

  let ( >>= ) = Thread.bind
  let ( >>| ) x f = x >>= fun x -> Thread.return (f x)

  let rec rev_map_p_aux ~f accu = function
    | [] -> accu
    | h :: t ->
      let f_h = f h in
      let accu' =
        accu >>= fun ys ->
        f_h >>| fun y ->
        y :: ys
      in
      rev_map_p_aux ~f accu' t

  let rev_map_p ~f xs = rev_map_p_aux ~f (Thread.return []) xs

  let map_p ~f xs =
    rev_map_p ~f xs >>| List.rev

  let rec fold_s xs ~init ~f =
    match xs with
    | [] -> Thread.return init
    | h :: t ->
      f init h >>= fun f_h ->
      fold_s t ~init:f_h ~f

  (* This implementation doesn't have the exact same semantics as
     [Lwt.join] and is less efficient (since it builds a list of unit
     values). Another option would be to include [map_p] and [join] in
     [Thread]. *)
  let join (xs : unit Thread.t list) = map_p ~f:ident xs >>| ignore

  (* Task as a graph vertex *)
  module V = struct
    type t = Task.t
    let compare u v = String.compare (Task.id u) (Task.id v)
    let hash u = Hashtbl.hash (Task.id u)
    let equal u v =
      Task.id u = Task.id v
  end

  (* Graph of tasks *)
  module G = struct
    include Graph.Persistent.Digraph.ConcreteBidirectional(V)
    let exists_pred g u ~f =
      let f v accu = accu || f v in
      fold_pred f g u false

    let predecessors g u = fold_pred (fun h t -> h :: t) g u []
    let successors   g u = fold_succ (fun h t -> h :: t) g u []
  end

  module Dfs = Graph.Traverse.Dfs(G)
  module Bfs = Graph.Traverse.Bfs(G)

  type t = G.t
  type task = Task.t
  type task_result = Task.result
  type 'a thread = 'a Thread.t
  type allocator = Allocator.t
  type resource = Allocator.resource
  type config = Task.config

  type trace =
    | Run of { ready : time ;
               start : time ;
               end_ : time ;
               outcome : task_result }

    | Skipped of [ `Done_already
                 | `Missing_dep
                 | `Allocation_error of string ]

  and time = float

  type event =
    | Init of t
    | Task_ready of task
    | Task_started of task * resource
    | Task_ended of task_result
    | Task_skipped of task * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string ]

  class type logger = object
    method event : config -> time -> event -> unit
    method stop : unit
    method wait4shutdown : unit thread
  end

  let nb_tasks = G.nb_vertex
  let mem_task = G.mem_vertex

  let empty = G.empty

  let add_task = G.add_vertex

  let add_dep g u ~on:v =
    G.add_edge g u v

  let dot_output g vertex_attributes edge_attributes fn =
    let module G = struct
      include G
      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name t = sprintf "\"%s\"" (Task.id t)
      let vertex_attributes = vertex_attributes
      let edge_attributes = edge_attributes
      let get_subgraph _ = None
      let default_edge_attributes _ = []
    end in
    let module Dot = Graph.Graphviz.Dot(G) in
    Out_channel.with_file fn ~f:(fun oc ->
        Dot.output_graph oc g
      )

  let sources g =
    let f u accu =
      if G.in_degree g u = 0 then u :: accu
      else accu
    in
    G.fold_vertex f g []

  (* Set of tasks *)
  module S = Caml.Set.Make(V)

  (* Traverses a DAG with given goals to determine which tasks are
     needed and which are done already. This a depth-first traversal,
     and the set of visited tasks coincides with tasks which are
     needed. *)
  let initial_state config g goals =
    let rec aux ((needed, already_done) as accu) u =
      if S.mem u needed then Thread.return accu
      else
        Task.is_done u config >>= fun u_is_done ->
        let accu = (S.add u needed,
                    if u_is_done then S.add u already_done else already_done)
        in
        if u_is_done then Thread.return accu
        else fold_s (G.successors g u) ~init:accu ~f:aux
    in
    fold_s goals ~init:(S.empty, S.empty) ~f:aux

  let successfull_trace = function
    | Run { outcome } -> not (Task.failure outcome)
    | Skipped `Done_already -> true
    | _ -> false

  (* [performance_thread config logger alloc dep_traces u] builds the
     threads that will actually /perform/ the execution of the task
     [u] given the results of the deps ([dep_traces]) *)
  let performance_thread config logger alloc dep_traces u =
      map_p ~f:ident dep_traces >>= fun dep_traces ->
      if List.for_all dep_traces ~f:successfull_trace then (
        let ready = Unix.gettimeofday () in
        logger#event config ready (Task_ready u) ;
        Allocator.request alloc (Task.requirement u) >>= function
        | Ok resource ->
          let start = Unix.gettimeofday () in
          logger#event config start (Task_started (u, resource)) ;
          Task.perform resource config u >>= fun outcome ->
          let end_ = Unix.gettimeofday () in
          logger#event config end_ (Task_ended outcome) ;
          Allocator.release alloc resource ;
          Thread.return (Run { ready ; start ; end_ ; outcome })
        | Error (`Msg msg) ->
          let err = `Allocation_error msg in
          logger#event config (Unix.gettimeofday ()) (Task_skipped (u, err)) ;
          Thread.return (Skipped err)
      )
      else (
        logger#event config (Unix.gettimeofday ()) (Task_skipped (u, `Missing_dep)) ;
        Thread.return (Skipped `Missing_dep)
      )

  let performance_table config logger alloc ~needed ~already_done g sources =
    let module M = String.Map in
    let rec aux u ((seen, table) as accu) =
      if S.mem u seen then accu
      else
        let seen, table = G.fold_succ aux g u accu in
        (* FIXME: this is fishy, because if [u] is not needed, why *)
        (* recurse on its deps? *)
        let is_needed = S.mem u needed in
        let is_done = S.mem u already_done in
        let thread =
          if not is_needed then (
            Thread.return (Skipped `Done_already) (* FIXME *)
          )
          else if is_done then (
            logger#event config (Unix.gettimeofday ()) (Task_skipped (u, `Done_already)) ;
            Thread.return (Skipped `Done_already)
          )
          else
            let dep_traces =
              let f u accu = M.find_exn table (Task.id u) :: accu in
              G.fold_succ f g u []
            in
            performance_thread config logger alloc dep_traces u
        in
        S.add u seen,
        M.add table ~key:(Task.id u) ~data:thread
    in
    List.fold sources ~init:(S.empty, M.empty) ~f:(Fn.flip aux)
    |> snd


  let post_revdeps_actions config g thread_table =
    let foreach_task u accu =
      let f v accu =
        match String.Map.find thread_table (Task.id v) with
        | Some t -> (t >>| successfull_trace) :: accu
        | None -> accu
      in
      let thread =
        G.fold_pred f g u []
        |> map_p ~f:ident >>= fun res ->
        let all_revdeps_succeeded = List.for_all ~f:ident res in
        Task.post_revdeps_hook u config ~all_revdeps_succeeded
      in
      thread :: accu
    in
    G.fold_vertex foreach_task g []
    |> join

  let null_logger = object
    method event _ _ _ = ()
    method stop = ()
    method wait4shutdown = Thread.return ()
  end

  let run ?(logger = null_logger) ?goals config alloc g =
    if Dfs.has_cycle g then failwith "Cycle in dependency graph" ;
    let goals = match goals with
      | None -> sources g
      | Some tasks -> tasks
    in
    logger#event config (Unix.gettimeofday ()) (Init g) ;
    initial_state config g goals >>= fun (needed, already_done) ->
    let performance_table =
      performance_table config logger alloc ~needed ~already_done g goals
    in
    let ids, threads =
      performance_table
      |> String.Map.to_alist
      |> List.unzip
    in
    post_revdeps_actions config g performance_table >>= fun () ->
    map_p threads ~f:ident >>| fun traces ->
    List.zip_exn ids traces
    |> String.Map.of_alist_exn
end
