open Core_kernel.Std

val run :
  Task.config ->
  Allocator.t ->
  Bistro.any_workflow list ->
  Tdag_sig.trace String.Map.t Lwt.t
