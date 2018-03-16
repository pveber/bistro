open Core

type t = Bistro.u

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type result =
  | Input_check of { path : string ; pass : bool }
  | Select_check of { dir_path : string ; sel : string list ; pass : bool }
  | Step_result of {
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      step : Bistro.step ;
      exit_code : int ;
      action : [`Sh of string | `Eval] ;
      file_dumps : file_dump list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }
  | Map_command_result of {
      pass : bool ;
    }

type config = private {
  db : Db.t ;
  use_docker : bool ;
  keep_all : bool ;
  precious : String.Set.t ;
}

val config :
  db_path:string ->
  use_docker:bool ->
  keep_all:bool ->
  precious:String.Set.t ->
  config

val id : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val requirement : t -> Allocator.request
val perform : Allocator.resource -> config -> t -> result Lwt.t
val failure : result -> bool
val is_done : t -> config -> bool Lwt.t
val post_revdeps_hook :
  t ->
  config ->
  all_revdeps_succeeded:bool ->
  unit Lwt.t
val clean : t -> config -> unit Lwt.t
