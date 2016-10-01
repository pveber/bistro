module type Task = sig
  type t
  type request
  type resource

  val id : t -> string
  val requirement : t -> request
  val perform : resource -> t -> unit
  val clean : t -> unit
end

module type Allocator = sig
  type t
  type request
  type resource

  val request : t -> request -> resource
  val free : t -> resource -> unit
end

module type S = sig
  type t
  type task

  val empty : t
  val add_task : t -> task -> t
  val add_dep : t -> task -> on:task -> t

  val run : t -> unit Lwt.t
end
