type 'a workflow

val cached_value :
  ?descr:string ->
  (unit -> 'a) workflow ->
  'a workflow

type 'a path

val input : string -> 'a path workflow

val cached_path :
  ?descr:string ->
  (string -> unit) workflow ->
  'a path workflow


class type directory = object
  method file_kind : [`directory]
end

val select :
  #directory path workflow ->
  string list ->
  'a path workflow


val pure : id:string -> 'a -> 'a workflow
val pure_data : 'a -> 'a workflow
val app : ('a -> 'b) workflow -> 'a workflow -> 'b workflow
val both : 'a workflow -> 'b workflow -> ('a * 'b) workflow

val eval_path : 'a path workflow -> string workflow

val spawn :
  'a list workflow ->
  f:('a workflow -> 'b workflow) ->
  'b list workflow


