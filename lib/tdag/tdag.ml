open Tdag_sig

module Make(D : Domain) = struct
  open D

  module V = struct
    type t = Task.t
    let compare u v = String.compare (Task.id u) (Task.id v)
    let hash u = Hashtbl.hash (Task.id u)
    let equal u v =
      Task.id u = Task.id v
  end

  module G = Graph.Persistent.Digraph.Concrete(V)

  type t = G.t
  type task = Task.t
  type 'a thread = 'a Thread.t

  let empty = G.empty

  let add_task = G.add_vertex

  let add_dep g u ~on:v =
    G.add_edge g u v

  let run _ = assert false
end
