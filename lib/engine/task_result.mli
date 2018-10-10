type t =
  | Input of { id : string ; path : string ; pass : bool }
  | Select of { id : string ; dir_path : string ; sel : string list ; pass : bool }
  | Shell of {
      id : string ;
      descr : string ;
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      exit_code : int ;
      cmd : string ;
      file_dumps : Shell_command.file_dump list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }
  | Plugin of {
      id : string ;
      descr : string ;
      outcome : [`Succeeded | `Missing_output | `Failed] ;
    }
  | Collect_in_directory of { id : string ; pass : bool }

val id : t -> string

val succeeded : t -> bool

val error_short_descr : t -> string
(** @raise [Invalid_argument] is [succeeded r] *)

val error_long_descr : t -> Db.t -> Buffer.t -> string -> unit
