open Core_kernel.Std

type event =
  | Task_ready of Task.t
  | Task_started of Task.t
  | Task_ended of Task.t * unit Tdag_sig.result
  | Task_skipped of Task.t * [`Done_already | `Missing_dep]
and time = float

val run :
  ?log:(time -> event -> unit) ->
  Task.config ->
  Allocator.t ->
  Bistro.any_workflow list ->
  Tdag_sig.trace String.Map.t Lwt.t
