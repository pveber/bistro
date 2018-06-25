open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

val requirement : _ Workflow.t -> Allocator.request

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
