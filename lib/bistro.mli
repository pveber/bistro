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

module Internals : sig
  module Workflow : sig
    type _ t =
      | Pure : { id : string ; value : 'a } -> 'a t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Both : 'a t * 'b t -> ('a *'b) t
      | Eval_path : string t -> string t
      | Spawn : {
          elts : 'a list t ;
          f : 'a t -> 'b t ;
        } -> 'b list t

      | Input : { id : string ; path : string } -> string t
      | Select : {
          id : string ;
          dir : #directory path t ;
          sel : string list ;
        } -> string t
      | Value : (unit -> 'a) step -> 'a t
      | Path : (string -> unit) step -> string t

    and 'a step = {
      id : string ;
      descr : string ;
      workflow : 'a t ;
    }
    val reveal : 'a workflow -> 'a t
  end
end
