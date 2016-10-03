type 'a result = ('a, [`Msg of string]) Pervasives.result

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

    val request : t -> request -> resource Thread.t
    val free : t -> resource -> unit
  end

  module Task : sig
    type t

    val id : t -> string
    val requirement : t -> Allocator.request
    val perform : Allocator.resource -> t -> unit result Thread.t
    val is_done : t -> bool Thread.t
    val clean : t -> unit Thread.t
  end

end

module type S = sig
  type t
  type task
  type allocator
  type 'a thread

  val empty : t
  val add_task : t -> task -> t
  val add_dep : t -> task -> on:task -> t

  val run : allocator -> t -> unit thread
end
