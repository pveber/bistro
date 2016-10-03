open Tdag_sig

module Make(T : Task)(A : Allocator)(Thread : Thread) = struct
  module V = struct
    type t = T.t
    let compare u v = String.compare (T.id u) (T.id v)
    let hash u = Hashtbl.hash (T.id u)
    let equal u v =
      T.id u = T.id v
  end

  module G = Graph.Persistent.Digraph.Concrete(V)

  type t = G.t
  type task = T.t
  type 'a thread = 'a Thread.t

  let empty = G.empty

  let add_task = G.add_vertex

  let add_dep g u ~on:v =
    G.add_edge g u v

  let run _ = assert false
end
