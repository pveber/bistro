open Core_kernel.Std
open Rresult

module type Domain = sig

  module Thread : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a ->'b t) -> 'b t
  end

  module Allocator : sig
    type t
    type request
    type resource

    val request : t -> request -> (resource, R.msg) result Thread.t
    val release : t -> resource -> unit
  end

  module Task : sig
    type t
    type config
    type result

    val id : t -> string
    val requirement : t -> Allocator.request
    val perform :
      Allocator.resource ->
      config ->
      t ->
      result Thread.t
    val failure : result -> bool
    val is_done : config -> t -> bool Thread.t
  end

end

module type S = sig
  type t
  type task
  type task_result
  type allocator
  type resource
  type config
  type 'a thread

  type trace =
    | Run of { ready : time ;
               start : time ;
               end_ : time ;
               outcome : task_result }

    | Skipped of [ `Done_already
                 | `Missing_dep
                 | `Allocation_error of string ]

  and time = float

  type event =
    | Init of t
    | Task_ready of task
    | Task_started of task * resource
    | Task_ended of task_result
    | Task_skipped of task * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string ]

  class type logger = object
    method event : config -> time -> event -> unit
    method stop : unit
    method wait4shutdown : unit thread
  end

  val nb_tasks : t -> int
  val mem_task : t -> task -> bool

  val empty : t
  val add_task : t -> task -> t
  val add_dep : t -> task -> on:task -> t
  val dot_output :
    t ->
    (task -> Graph.Graphviz.DotAttributes.vertex list) ->
    (task * task -> Graph.Graphviz.DotAttributes.edge list) ->
    string ->
    unit

  val run :
    ?logger:logger ->
    config ->
    allocator ->
    t ->
    trace String.Map.t thread
end
