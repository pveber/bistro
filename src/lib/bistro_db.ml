open Core.Std

type t = string

let make x = x

let cache_dir base = Filename.concat base "cache"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"
let log_dir base = Filename.concat base "logs"
let history_dir base = Filename.concat base "history"

let setup base =
  Unix.mkdir_p (tmp_dir base) ;
  Unix.mkdir_p (cache_dir base) ;
  Unix.mkdir_p (stderr_dir base) ;
  Unix.mkdir_p (stdout_dir base) ;
  Unix.mkdir_p (log_dir base) ;
  Unix.mkdir_p (history_dir base)

let aux_path f db w =
  Filename.concat (f db) (Bistro_workflow.digest w)

let cache_path db w = aux_path cache_dir db w
let log_path db w = aux_path log_dir db w
let tmp_path db w = aux_path tmp_dir db w
let stdout_path db w = aux_path stdout_dir db w
let stderr_path db w = aux_path stderr_dir db w

let rec path db = Bistro_workflow.(function
  | Input p -> p
  | Select (dir, p) ->
    Filename.concat (path db dir) p
  | Rule r as w ->
    cache_path db w
)


type 'a logger = [ `debug | `info | `warning | `error ] -> ('a,unit,string,unit) format4 -> 'a

let logger (type s) oc level (fmt : (s, unit, string, unit) format4) =
  let open Unix in
  let open Printf in
  let label = match level with
    | `info -> "INFO"
    | `debug -> "DEBUG"
    | `warning -> "WARNING"
    | `error -> "ERROR"
  in
  let f msg =
    let t = localtime (time ()) in
      fprintf
        oc "[%s][%04d-%02d-%02d %02d:%02d] %s\n%!"
        label (1900 + t.tm_year) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min msg
  in
  ksprintf f fmt

let with_logger db w ~f =
  let filename = log_path db w in
  Out_channel.with_file filename ~f:(fun oc ->
    f (logger oc)
  )
