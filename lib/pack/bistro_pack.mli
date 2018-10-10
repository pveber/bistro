include module type of struct include Bistro end

module Expr : sig
  type 'a t

  val pure : 'a -> 'a t

  val glob :
    ?pattern:string ->
    _ #directory workflow t ->
    _ workflow list t

  val glob_full :
    ?pattern:string ->
    _ #directory workflow t ->
    (string * _ workflow) list t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val list_map : 'a list t -> f:('a -> 'b) -> 'b list t
  val spawn : 'a list t -> f:('a t -> 'b t) -> 'b list t
end

type 'a expr = 'a Expr.t

type logger = Bistro_engine.Logger.t
val null_logger : unit -> logger
val console_logger : unit -> logger

module Repo : Bistro_base.Sigs.Repo with type 'a workflow := 'a workflow
                                     and type logger := logger

(* val eval_expr :
 *   ?np:int ->
 *   ?mem:[`GB of int] ->
 *   ?loggers:logger list ->
 *   ?use_docker:bool ->
 *   ?bistro_dir:string ->
 *   'a expr -> ('a, string) result
 * 
 * val eval_expr_exn :
 *   ?np:int ->
 *   ?mem:[`GB of int] ->
 *   ?loggers:logger list ->
 *   ?use_docker:bool ->
 *   ?bistro_dir:string ->
 *   'a expr -> 'a *)
