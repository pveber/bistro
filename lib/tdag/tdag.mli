open Tdag_sig

module Make(T : Task)
           (A : Allocator with type request = T.request
                           and type resource = T.resource)
           (Thread : Thread with type 'a t = 'a T.thread) :
  S with type task = T.t
