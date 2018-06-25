open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t

val input :
  id:string ->
  path:string ->
  t

val select :
  dir:_ Workflow.t ->
  path:string list ->
  t

val shell :
  string Command.t -> t

val requirement : t -> Allocator.request

val perform : t -> Task_result.t Lwt.t

val is_done : _ Workflow.t -> Db.t -> bool Lwt.t

val perform_input :
  string -> Task_result.t Lwt.t

val perform_select :
  Db.t ->
  dir:_ Workflow.t ->
  sel:Path.t ->
  Task_result.t Lwt.t

val perform_shell :
  config ->
  Allocator.resource ->
  Workflow.shell ->
  Task_result.t Lwt.t

(* val perform_map_dir :
 *   Db.t ->
 *   files_in_dir:string list ->
 *   goals:_ Workflow.t list ->
 *   _ Workflow.t ->
 *   Task_result.t Lwt.t *)
