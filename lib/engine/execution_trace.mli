type time = float

module Outcome : sig
  type t = [
    | `Succeeded
    | `Missing_output
    | `Error_exit_code of int
    | `Failure of string option
  ]

  val is_success : t -> bool
end

module Run_details : sig
  type t =
    | Input of { id : string ; path : string ; pass : bool }
    | Select of { id : string ; dir_path : string ; sel : string list ; pass : bool }
    | Shell of {
        id : string ;
        descr : string ;
        outcome : Outcome.t ;
        cmd : string ;
        file_dumps : Shell_command.file_dump list ;
        cache : string option ;
        stdout : string ;
        stderr : string ;
      }
    | Plugin of {
        id : string ;
        descr : string ;
        outcome : Outcome.t ;
      }
    | Container_image_fetch of {
        id : string ;
        outcome : (unit, [ `Singularity_failed_pull of int * string ]) result
      }

  val id : t -> string

  val name : t -> string

  val succeeded : t -> bool

  val error_short_descr : t -> string
  (** @raise [Invalid_argument] is [succeeded r] *)

  val error_long_descr : t -> Db.t -> Buffer.t -> string -> unit
end

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             details : Run_details.t }

  | Done_already of { id : string }
  | Canceled of {
      id : string ;
      missing_deps : t list ;
    }
  | Allocation_error of {
      id : string ;
      msg : string ;
    }

val is_errored : t -> bool

val error_report :
  t ->
  Db.t ->
  Buffer.t ->
  unit

val all_ok : t list -> bool

val gather_failures : t list -> t list

module Set : Set.S with type elt = t
