open Core_kernel.Std
open Rresult

module DAG : sig
  type t
  type task = Task.t

  val dot_output : t -> string -> unit
end


type time = float

type event =
  | Init of DAG.t
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

val compile : Bistro.any_workflow list -> DAG.t

val run :
  ?log:(time -> event -> unit) ->
  Task.config ->
  Allocator.t ->
  DAG.t ->
  trace String.Map.t Lwt.t
