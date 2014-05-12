open Core.Std

type level = [ `debug | `info | `warning | `error ]

let string_of_level = function
  | `info -> "INFO"
  | `debug -> "DEBUG"
  | `warning -> "WARNING"
  | `error -> "ERROR"

type event = [
| `started_build of Bistro_workflow.u
| `finished_build of Bistro_workflow.u
| `failed_build of Bistro_workflow.u * string option
| `msg of level * string
]

type timestamp = Core.Std.Time.t

let string_of_timestamp t =Time.format t "%Y-%m-%d %H:%M:%S"

module Entry = struct
  type t = event * timestamp

  let to_string (ev, t) = match ev with
    | `msg (level, msg) ->
      sprintf
        "[%s][%s] %s"
        (string_of_timestamp t) (string_of_level level) msg
    | `started_build u ->
      sprintf
        "[%s] Started building %s"
        (string_of_timestamp t) (Bistro_workflow.digest u)
    | `finished_build u ->
       sprintf
         "[%s] Finished building %s"
         (string_of_timestamp t) (Bistro_workflow.digest u)

    | `failed_build (u, msg) ->
       sprintf
         "[%s] Build of %s failed!%s"
         (string_of_timestamp t)
         (Bistro_workflow.digest u)
         (Option.value_map msg ~default:"" ~f:(fun x -> "\n" ^ x))

end


type t = Entry.t -> unit

let make ?db ?hook () : t =
  let db_log = Option.value_map db ~default:ignore ~f:(fun db e ->
      Bistro_db.log db "%s" (Entry.to_string e)
    ) in
  let hook = Option.value hook ~default:ignore in
  fun e -> db_log e ; hook e

let started_build log u =
  log (`started_build u, Time.now ())

let finished_build log u =
  log (`finished_build u, Time.now ())

let failed_build log ?msg u =
  log (`failed_build (u, msg), Time.now ())

let logger (type s) log level (fmt : (s, unit, string, unit) format4) =
  let open Printf in
  let f msg = log (`msg (level, msg), Time.now ()) in
  ksprintf f fmt

let debug log fmt = logger log `debug fmt
let info log fmt = logger log `info fmt
let warning log fmt = logger log `warning fmt
let error log fmt = logger log `error fmt

