type result = Pbs.Qstat.t
type error = [
  | `Failure of string
  | `Qsub_failure of string * int
  | `Qstat_failure of string * int
  | `Qstat_wrong_output of string
]

val submit :
  queue:string ->
  Pbs.Script.t ->
  [ `Ok of result
  | `Error of error ] Lwt.t
