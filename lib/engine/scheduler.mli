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
val build_all : t -> any_workflow list -> string result list Lwt.t
val shutdown : t -> unit Lwt.t


(** LOW-LEVEL API *)
type execution_env = private {
  use_docker : bool ;
  dest : string ;
  tmp : string ;
  script_tmp : Task.script -> string ;
  dep : Task.dep -> string ;
  np : int ;
  mem : int ;
}

val make_execution_env :
  tmpdir:string ->
  use_docker:bool ->
  np:int ->
  mem:int ->
  Db.t ->
  execution_env

module Concrete_task : sig
  type t = private
    | Sh of string
    | Run_script of run_script
    | Dump of dump
    | And of t list
    | Or of t list
    | Pipe of t list

  and run_script = {
    cmd  : string ;
    text : string ;
    path : string ;
  }

  and dump = {
    dump_dest : string  ;
    dump_contents : string ;
  }

  val of_task_cmd : execution_env -> Task.cmd -> t
  val to_cmd : t -> string
end
