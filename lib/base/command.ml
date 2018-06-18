open Core_kernel

type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

let docker_image ?tag ?registry ~account ~name () = {
  dck_account = account ;
  dck_name = name ;
  dck_tag = tag ;
  dck_registry = registry ;
}

type 'a t =
  | Docker of docker_image * 'a t
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
  | Docker (_, c) -> deps c

let rec map x ~f = match x with
  | Docker (im, cmd) -> Docker (im, map ~f cmd)
  | Simple_command toks ->
    Simple_command (Template.map ~f toks)
  | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
  | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
  | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))

let rec uses_docker = function
  | Docker (_, _) -> true
  | Simple_command _ -> false
  | And_list xs
  | Or_list xs
  | Pipe_list xs -> List.exists xs ~f:uses_docker
