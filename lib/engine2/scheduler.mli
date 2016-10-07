open Core_kernel.Std

val run :
  Task.config ->
  Allocator.t ->
  Bistro.any_workflow list ->
  Tdag_sig.trace list Lwt.t
