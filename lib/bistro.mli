type 'a workflow

type 'a path

class type directory = object
  method file_kind : [`directory]
end

module Workflow : sig
  val cached_value :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    ?version:int ->
    (unit -> 'a) workflow ->
    'a workflow

  val input :
    ?version:int ->
    string -> 'a path workflow

  val cached_path :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    ?version:int ->
    (string -> unit) workflow ->
    'a path workflow

  val select :
    #directory path workflow ->
    string list ->
    'a path workflow


  val pure : id:string -> 'a -> 'a workflow
  val pure_data : 'a -> 'a workflow
  val int : int -> int workflow
  val string : string -> string workflow
  val app : ('a -> 'b) workflow -> 'a workflow -> 'b workflow
  val both : 'a workflow -> 'b workflow -> ('a * 'b) workflow

  val eval_path : 'a path workflow -> string workflow

  val spawn :
    'a list workflow ->
    f:('a workflow -> 'b workflow) ->
    'b list workflow
end

module Private : sig
  val reveal : 'a workflow -> 'a Bistro_internals.Workflow.t
end
