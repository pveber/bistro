open Tdag_sig

module Make(D : Domain) : S with type task = D.Task.t
                             and type 'a thread = 'a D.Thread.t
                             and type allocator = D.Allocator.t
                             and type config = D.Task.config
                             and type task_result = D.Task.result
