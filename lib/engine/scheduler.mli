type error = [
  | `Msg of string
]

module Gc : sig
  open Bistro_internals

  type state = {
    deps : (Workflow.any * Workflow.any) list ;
    protected : Workflow.any list ;
  }
end

module type Backend = sig
  open Bistro_internals

  type t
  type token

  val run_shell_command :
    t ->
    token ->
    Shell_command.t ->
    (int * bool, string) Lwt_result.t

  val eval :
    t ->
    token ->
    ('a -> unit) ->
    'a ->
    (unit, string) Lwt_result.t

  val build_trace :
    t ->
    _ Workflow.t ->
    Allocator.request ->
    (token -> Allocator.resource -> Execution_trace.Run_details.t Eval_thread.t) ->
    Execution_trace.t Eval_thread.t

  val stop : t -> unit Lwt.t
end

module Make(Backend : Backend) : sig
  type t

  val create :
    ?allowed_containers:[`Docker | `Singularity] list ->
    ?loggers:Logger.t list ->
    ?collect:bool ->
    Backend.t ->
    Db.t ->
    t

  val gc_state : t -> Gc.state option

  val protect : t -> _ Bistro.workflow -> unit

  val start : t -> unit

  val eval :
    t ->
    'a Bistro.workflow ->
    ('a, Execution_trace.t list) Lwt_result.t

  val eval_exn :
    t ->
    'a Bistro.workflow ->
    'a Lwt.t

  val error_report :
    t ->
    Execution_trace.t list ->
    string

  val stop : t -> unit Lwt.t
end

include module type of Make(Local_backend)

val create :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  Db.t ->
  t

val simple_eval_exn :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  ?db_path:string ->
  'a Bistro.workflow ->
  'a
