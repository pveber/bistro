type 'a path = Path of string

type _ workflow =
  | Pure : 'a code -> 'a workflow
  | App : ('a -> 'b) code * 'a code -> 'b workflow
  | Cached_value : (unit -> 'a) workflow -> 'a workflow
  | Cached_path : (string -> unit) workflow -> 'a path workflow

let eval : 'a. 'a workflow -> 'a code = function
  | Pure c -> c


