open Tdag_sig

module Make(T : Task)
           (A : Allocator with type request = T.request
                           and type resource = T.resource) :
  S with type task = T.t
