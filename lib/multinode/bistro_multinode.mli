open Core_kernel
open Bistro
open Bistro_engine

module Client : sig
  val command : Command.t
end

module Server : sig
  type t

  val create :
    ?allowed_containers:[`Docker | `Singularity] list ->
    ?loggers:Logger.t list ->
    ?collect:bool ->
    ?port:int ->
    Db.t ->
    t Lwt.t

  val start : t -> unit

  val eval :
    t ->
    'a Bistro.workflow ->
    ('a, Execution_trace.t list) Lwt_result.t

  val stop : t -> unit Lwt.t

  val simple_app :
    ?allowed_containers:[`Docker | `Singularity] list ->
    ?loggers:Logger.t list ->
    ?collect:bool ->
    ?port:int ->
    ?db:string ->
    'a workflow ->
    unit

  val simple_command :
    summary:string ->
    'a workflow ->
    Command.t
end
