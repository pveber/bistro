(** Lwt threads that accumulate errors *)

type 'a t = ('a, Execution_trace.Set.t) Lwt_result.t
val return : 'a -> 'a t
(* val fail : Traces.t -> 'a t *)
val fail1 : Execution_trace.t -> 'a t
val both : 'a t -> 'b t -> ('a * 'b) t
(* val list_map : *)
(*   'a list -> *)
(*   f:('a -> 'b t) -> *)
(*   'b list t *)
val join2 : unit t -> unit t -> unit t
val join :
  'a list ->
  f:('a -> unit t) ->
  unit t
val ignore : 'a t -> unit t
module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >> ) : 'a Lwt.t -> ('a -> 'b t) -> 'b t
end
