module Time = Core.Std.Time

type event = [
| `started_build of Bistro_workflow.u
| `finished_build of Bistro_workflow.u
| `msg of level * string
]
and level = [ `debug | `info | `warning | `error ]
and timestamp = Core.Std.Time.t

type t = {
  evt : (timestamp * event) React.event ;
  send : timestamp -> event -> unit ;
}

let make () =
  let evt, send = React.E.create () in
  { evt ; send = fun x y -> send (x,y) }

let event log = log.evt

let started log u =
  log.send (Time.now ()) (`started_build u)

let finished log u =
  log.send (Time.now ()) (`finished_build u)

let logger (type s) log level (fmt : (s, unit, string, unit) format4) =
  let open Printf in
  let f msg = log.send (Time.now ()) (`msg (level, msg)) in
  ksprintf f fmt

let debug log fmt = logger log `debug fmt
let info log fmt = logger log `info fmt
let warning log fmt = logger log `warning fmt
let error log fmt = logger log `error fmt

let log_msg level t msg =
  let open Unix in
  let open Printf in
  let label = match level with
    | `info -> "INFO"
    | `debug -> "DEBUG"
    | `warning -> "WARNING"
    | `error -> "ERROR"
  in
  sprintf "[%s][%s] %s" label (Core.Std.Time.to_string t) msg

let to_strings log =
  let msg t = function
    | `started_build u ->
      log_msg `info t (Printf.sprintf "Started building %s" (Bistro_workflow.digest u))
    | `finished_build u ->
      log_msg `info t (Printf.sprintf "Finished building %s" (Bistro_workflow.digest u))
    | `msg (level, msg) -> log_msg level t msg
  in
  React.E.map (fun (t,e) -> msg t e) log.evt
