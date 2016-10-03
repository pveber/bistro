open Tdag_sig

module Make(D : Domain) : S with type task = D.Task.t
                             and type 'a thread = 'a D.Thread.t
