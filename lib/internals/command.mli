module Docker_image : sig
  type t = {
    account : string ;
    name : string ;
    tag : string option ;
    registry : string option ;
  }
end

module Singularity_image : sig
  type t = {
    account : string ;
    name : string ;
    tag : string option ;
    registry : string option ;
  }
end

type container_image =
  | Docker_image of Docker_image.t
  | Singularity_image of Singularity_image.t

val docker_image :
  ?tag:string ->
  ?registry:string ->
  account:string ->
  name:string ->
  unit -> container_image
(** Construct a description of a publicly available docker image *)

type 'a t =
  | Within_container of container_image list * 'a t
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

val map :
  'a t ->
  f:('a -> 'b) ->
  'b t

val deps : 'a t -> 'a list
val uses_container : 'a t -> bool
