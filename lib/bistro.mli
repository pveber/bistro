type 'a workflow
type 'a expr
type 'a path

class type directory = object
  method file_kind : [`directory]
end

module Workflow : sig
  type 'a t = 'a workflow
  val input : string -> 'a path workflow

  val make :
    ?descr:string ->
    'a expr ->
    'a t

  val makep :
    ?descr:string ->
    (string -> unit) expr ->
    'a path t

  val select :
    #directory path t ->
    string list ->
    'a path t

end

module Expr : sig
  type 'a t = 'a expr

  val pure : id:string -> 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t

  val eval_workflow : 'a workflow -> 'a t
  val eval_path : 'a path workflow -> string t

  val spawn :
    'a list t ->
    f:('a t -> 'b t) ->
    'b list t
end


