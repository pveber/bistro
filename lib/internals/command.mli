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

module Guix_environment : sig
  type package = {
    name : string ;
    version : string ;
  }
  type t = package list
end

type container_image = [
  | `Docker_image of Docker_image.t
  | `Singularity_image of Singularity_image.t
]

type env = [
  container_image
| `Guix_environment of Guix_environment.t
]

val docker_image :
  ?tag:string ->
  ?registry:string ->
  account:string ->
  name:string ->
  unit -> env
(** Construct a description of a publicly available docker image *)

type 'a t =
  | Within_env of env list * 'a t
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
