open Core_kernel
open Bistro_base

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Done_already
  | Canceled of { missing_deps : String.Set.t }
  | Allocation_error of string

val is_errored : t -> bool

val error_report :
  t ->
  Db.t ->
  Buffer.t ->
  string ->
  unit

val all_ok : t list -> bool

val gather_failures :
  Workflow.u list ->
  t list ->
  String.Set.t
