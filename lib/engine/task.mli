type t =
  | Input of string * path
  | Select of string * [`Input of path | `Step of string] * path
  | Step of step

and step = {
  id      : id ;
  descr   : string ;
  deps    : dep list ;
  cmd     : command ;
  np      : int ; (** Required number of processors *)
  mem     : int ; (** Required memory in MB *)
  timeout : int option ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
}

and dep = [
    `Task of id
  | `Select of id * path
  | `Input of path
]
and id = string

and command =
  | Docker of Bistro.docker_image * command
  | Simple_command of token list
  | And_list of command list
  | Or_list of command list
  | Pipe_list of command list

and token =
  | S of string
  | D of dep
  | F of token list
  | DEST
  | TMP
  | NP
  | MEM

and path = string list
[@@deriving sexp]

type error =
  | Input_doesn't_exist of string
  | Invalid_select of string * path
  | Step_failure of {
      exit_code : int ;
      script : string ;
      dumps : (string * string) list ;
    }

type config = private {
  db : Db.t ;
  use_docker : bool ;
}

val config :
  db_path:string ->
  use_docker:bool ->
  config

val of_workflow : Bistro.u -> t
val id : t -> string
val requirement : t -> Allocator.request
val perform : Allocator.resource -> config -> t -> (unit, error) result Lwt.t
val is_done : config -> t -> bool Lwt.t
val clean : config -> t -> unit Lwt.t

(* LOW-LEVEL API *)
val render_step_command : np:int -> mem:int -> config -> step -> string
val render_step_dumps : np:int -> mem:int -> config -> step -> (string * string) list
