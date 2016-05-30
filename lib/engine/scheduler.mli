open Core.Std
open Bistro

type error = (Task.dep * string) list
type 'a result = ('a, error) Result.t

type execution_report = {
  script : string ;
  exit_status : int ;
}

type backend =
  Db.t -> Task.t -> execution_report Lwt.t

val local_backend :
  ?tmpdir:string ->
  ?use_docker:bool ->
  np:int -> mem:int -> unit -> backend


type t

val make : backend -> Db.t -> t

(** [build w] runs the execution of the workflow [w] and returns the
    path of the result or an error *)
val build : t -> _ workflow -> string result Lwt.t
val build_exn : t -> _ workflow -> string Lwt.t

val shutdown : t -> unit Lwt.t
