open Core.Std
open Bistro

type error = (Workflow.u * string) list
type 'a result = ('a, error) Result.t

type execution_report = {
  script : string ;
  exit_status : int ;
}

type backend =
  Db.t -> Workflow.step -> execution_report Lwt.t

val local_backend :
  ?tmpdir:string ->
  ?use_docker:bool ->
  np:int -> mem:int -> unit -> backend


type t

val make : backend -> Db.t -> t

(** [build w] runs the execution of the workflow [w] and returns the
    path of the result or an error *)
val build : t -> _ Workflow.t -> string result Lwt.t
val build_exn : t -> _ Workflow.t -> string Lwt.t

(** [build] variants for untyped workflows. *)
val build' : t -> Workflow.u -> string result Lwt.t
val build_exn' : t -> Workflow.u -> string Lwt.t

val shutdown : t -> unit Lwt.t
