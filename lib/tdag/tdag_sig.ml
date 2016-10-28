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
    type error

    val id : t -> string
    val requirement : t -> Allocator.request
    val perform :
      Allocator.resource ->
      config ->
      t ->
      (unit, error) Pervasives.result Thread.t
    val is_done : config -> t -> bool Thread.t
    val clean : config -> t -> unit Thread.t
  end

end

module type S = sig
  type t
  type task
  type task_error
  type allocator
  type config
  type 'a thread

  type trace =
    | Run of { ready : time ;
               start : time ;
               end_ : time ;
               outcome : (unit, task_error) result }

    | Skipped of [ `Done_already
                 | `Missing_dep
                 | `Allocation_error of string ]

  and time = float

  type event =
    | Init of t
    | Task_ready of task
    | Task_started of task
    | Task_ended of task * (unit, task_error) result
    | Task_skipped of task * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string ]

  class type logger = object
    method event : time -> event -> unit
    method stop : unit
    method wait4shutdown : unit thread
  end

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
