open Core_kernel.Std
open Rresult

type dag

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

val compile : Bistro.any_workflow list -> dag

val dag_dot_output : dag -> string -> unit

val run :
  ?log:(time -> event -> unit) ->
  Task.config ->
  Allocator.t ->
  dag ->
  trace String.Map.t Lwt.t
