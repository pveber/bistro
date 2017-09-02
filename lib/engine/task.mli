open Core

type t = Bistro.u

type result =
  | Input_check of { path : string ; pass : bool }
  | Select_check of { dir_path : string ; sel : string list ; pass : bool }
  | Step_result of {
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      step : Bistro.step ;
      exit_code : int ;
      action : [`Sh of string | `Eval] ;
      dumps : (string * string) list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
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

(* LOW-LEVEL API *)
val render_step_command :
  np:int ->
  mem:int ->
  config ->
  Bistro.step ->
  Bistro.dep Bistro.Command.t ->
  string

val render_step_dumps :
  np:int ->
  mem:int ->
  config ->
  Bistro.step ->
  (string * string) list
