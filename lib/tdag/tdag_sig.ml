module type Task = sig
  type t
  type request
  type resource
  type 'a thread

  val id : t -> string
  val requirement : t -> request
  val perform : resource -> t -> (unit, [`Msg of string]) result thread
  val is_done : t -> bool thread
  val clean : t -> unit thread
end

module type Allocator = sig
  type t
  type request
  type resource
  type 'a thread

  val request : t -> request -> resource thread
  val free : t -> resource -> unit
end

module type Thread = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a ->'b t) -> 'b t
end

module type S = sig
  type t
  type task
  type 'a thread

  val empty : t
  val add_task : t -> task -> t
  val add_dep : t -> task -> on:task -> t

  val run : t -> unit thread
end
