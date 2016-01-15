open Bistro

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

type error = (Workflow.u * string) list

type backend_error = [
  | `Script_failure
  | `Unsupported_interpreter
]

type backend =
  np:int ->
  mem:int ->
  timeout:int ->
  stdout:string ->
  stderr:string ->
  interpreter:interpreter ->
  script:string ->
  (unit, backend_error) result Lwt.t

val local_backend : np:int -> mem:int -> backend

val pbs_backend : queue:string -> backend


type t

val make : backend -> Db.t -> t

(** [build w] runs the execution of the workflow [w] and returns the
    path of the result or an error *)
val build : t -> _ Workflow.t -> (string, error) result Lwt.t
val build_exn : t -> _ Workflow.t -> string Lwt.t

(** [build] variants for untyped workflows. *)
val build' : t -> Workflow.u -> (string, error) result Lwt.t
val build_exn' : t -> Workflow.u -> string Lwt.t

val shutdown : t -> unit Lwt.t
