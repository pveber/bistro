open Core
open Bistro_internals
open Bistro_engine

module W = Workflow

module V = struct
  include W.Any
  let sexp_of_t _ = assert false
  let t_of_sexp _ = assert false
end

module E = struct
  type t = Dependency | GC_link
  let default = Dependency
  let compare = compare
end

module S = Set.Make(V)

module G = struct
  open E
  include Graph.Persistent.Digraph.ConcreteLabeled(V)(E)
  (* let successors   g u = fold_succ (fun h t -> h :: t) g u [] *)

  let rec of_workflow_aux seen acc u =
    if S.mem seen u then (seen, acc)
    else (
      let deps = W.Any.deps u in
      let seen, acc =
        List.fold deps
          ~init:(seen, acc)
          ~f:(fun (seen, acc) v -> of_workflow_aux seen acc v)
      in
      let acc = add_vertex acc u in
      let acc = List.fold deps ~init:acc ~f:(fun acc v -> add_edge acc u v) in
      let seen = S.add seen u in
      seen, acc
    )

  let of_workflow u =
    of_workflow_aux S.empty empty (W.Any u)
    |> snd

  let of_gc_state ?(condensed = false) { Scheduler.Gc.deps ; _ } =
    let backbone =
      if condensed then empty
      else
        List.fold deps ~init:(S.empty, empty) ~f:(fun (seen, acc) (u, v) ->
            let seen, acc = of_workflow_aux seen acc u in
            of_workflow_aux seen acc v
          )
        |> snd
    in
    List.fold deps ~init:backbone ~f:(fun acc (u, v) ->
        let e = E.create u GC_link v in
        add_edge_e acc e
      )

  let is_path (W.Any w) =
    match w with
    | Plugin _ -> true
    | Shell _ -> true
    | Input _ -> true
    | Select _ -> true
    | Pure _ -> false
    | App _ -> false
    | Eval_path _ -> false
    | Both _ -> false
    | List _ -> false
    | Spawn _ -> false
    | List_nth _ -> false
    | Glob _ -> false

  
  let reduce_to_paths g =
    let foreach_vertex v acc =
      if is_path v then acc else (
        let f p acc = fold_succ (fun s acc -> add_edge acc p s) g p acc in
        fold_pred f g v (remove_vertex g v)
      )
    in
    fold_vertex foreach_vertex g g
end


let light_gray = 0xC0C0C0
let black = 0

let shape = function
  | _ -> `Box

let dot_output ?db oc g ~needed =
  let already_done = match db with
    | None -> Fn.const false
    | Some db -> Db.is_in_cache db
  in
  let label descr u = `Label (sprintf "%s.%s" descr (String.prefix (W.Any.id u) 6)) in
  let step_attributes ~descr u =
    let already_done = already_done u in
    let color = black in
    let shape = `Shape (shape u) in
    [ label descr u ;
      shape ;
      `Peripheries (if already_done then 2 else 1) ;
      `Color color ;
      `Fontcolor color ;
    ]
  in
  let vertex_attributes u =
    let needed = db = None || S.mem needed u in
    let color = if needed then black else light_gray in
    let shape = `Shape (shape u) in
    let W.Any w = u in
    match w with
    | W.Input i ->
      let label = i.path in
      [ `Label label ; `Color color ; `Fontcolor color ; shape ]
    | Select s ->
      let label = Path.to_string s.sel in
      [ `Label label ; `Fontcolor color ; `Color color ; shape ]
    | Shell { descr ; _ } -> step_attributes ~descr u
    | Plugin { descr ; _ } -> step_attributes ~descr u
    | Pure _ -> [ label "pure" u ; `Shape `Plaintext ]
    | App _ -> [ label "app" u ; `Shape `Plaintext ]
    | Spawn _ -> [ label "spawn" u ; `Shape `Ellipse ]
    | Both _ -> [ label "both" u ; `Shape `Plaintext ]
    | List _ -> [ label "list" u ; `Shape `Plaintext ]
    | Glob _ -> [ label "glob" u ; `Shape `Plaintext ]
    | Eval_path _ -> [ label "path" u ; `Shape `Plaintext ]
    | List_nth l -> [ label (sprintf "list_nth_%d" l.index) u ; `Shape `Plaintext ] 
  in
  let edge_attributes e =
    let u = G.E.src e and v = G.E.dst e in
    let style = match u, v, G.E.label e with
      | _, _, GC_link -> [ `Style `Dotted ]
      | W.Any W.Select _, _, Dependency -> [ `Style `Dashed ]
      | _ -> []
    in
    let color =
      if db = None || (S.mem needed u && not (already_done u))
      then black else light_gray in
    style @ [ `Color color ]
  in
  let module G = struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name t = sprintf "\"%s\"" (W.Any.id t)
    let vertex_attributes = vertex_attributes
    let edge_attributes = edge_attributes
    let get_subgraph _ = None
    let default_edge_attributes _ = []
  end in
  let module Dot = Graph.Graphviz.Dot(G) in
  Dot.output_graph oc g

(* class logger path : Scheduler.logger =
 *   object
 *     method event config _ = function
 *       | Scheduler.Init { dag ; needed ; already_done } ->
 *         let needed = S.of_list needed in
 *         let already_done = S.of_list already_done in
 *         dot_output dag ~needed ~already_done path ~precious:config.Task.precious
 *       | _ -> ()
 *
 *     method stop = ()
 *
 *     method wait4shutdown = Lwt.return ()
 *   end
 *
 * let create path = new logger path *)

let workflow_to_channel ?db ?(reduce = false) oc w =
  let dep_graph = G.of_workflow (Bistro.Private.reveal w) in
  let dep_graph = if reduce then G.reduce_to_paths dep_graph else dep_graph in
  dot_output ~needed:S.empty ?db oc dep_graph

let workflow_to_file ?db ?reduce fn w =
  Out_channel.with_file fn ~f:(fun oc -> workflow_to_channel ?db ?reduce oc w)

let gc_state_to_channel ?condensed ?db oc gcs =
  let dep_graph = G.of_gc_state ?condensed gcs in
  dot_output ~needed:S.empty ?db oc dep_graph

let gc_state_to_file ?condensed ?db fn w =
  Out_channel.with_file fn ~f:(fun oc -> gc_state_to_channel ?condensed ?db oc w)
