open Core_kernel.Std
open Rresult

type time = float

type event =
  | Task_ready of Task.t
  | Task_started of Task.t
  | Task_ended of Task.t * (unit, Task.error) result
  | Task_skipped of Task.t * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string]

type trace =
  | Run of { ready : time ;
             start : time ;
             end_ : time ;
             outcome : (unit, Task.error) result }

  | Skipped of [ `Done_already
               | `Missing_dep
               | `Allocation_error of string ]

val run :
  ?log:(time -> event -> unit) ->
  Task.config ->
  Allocator.t ->
  Bistro.any_workflow list ->
  trace String.Map.t Lwt.t
