open Base

module Docker_image = struct
  type t = {
    account : string ;
    name : string ;
    tag : string option ;
    registry : string option ;
  }
end

module Singularity_image = struct
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

let docker_image ?tag ?registry ~account ~name () =
  Docker_image {
    account = account ;
    name = name ;
    tag = tag ;
    registry = registry ;
  }

type 'a t =
  | Within_container of container_image list * 'a t
  | Simple_command of 'a template
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

and 'a template = 'a Template.t


let rec deps = function
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:deps
    |> List.concat
    |> List.dedup_and_sort ~compare:Caml.compare
  | Simple_command tokens -> Template.deps tokens
  | Within_container (_, c) -> deps c

let rec map x ~f = match x with
  | Within_container (im, cmd) -> Within_container (im, map ~f cmd)
  | Simple_command toks ->
    Simple_command (Template.map ~f toks)
  | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
  | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
  | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))

let rec uses_container = function
  | Within_container (_, _) -> true
  | Simple_command _ -> false
  | And_list xs
  | Or_list xs
  | Pipe_list xs -> List.exists xs ~f:uses_container
