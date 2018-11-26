type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

val docker_image :
  ?tag:string ->
  ?registry:string ->
  account:string ->
  name:string ->
  unit -> docker_image
(** Construct a description of a publicly available docker image *)

type 'a t =
  | Docker of docker_image * 'a t
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

val map :
  'a t ->
  f:('a -> 'b) ->
  'b t

val deps : 'a t -> 'a list
val uses_docker : 'a t -> bool
