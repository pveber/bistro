open Base

type 'a t =
  | Simple_command of 'a template
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

and 'a template = 'a Template.t


let rec deps cmd ~compare =
  match cmd with
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:(deps ~compare)
    |> List.concat
    |> List.dedup_and_sort ~compare
  | Simple_command tokens -> Template.deps tokens ~compare

let rec map x ~f = match x with
  | Simple_command toks ->
    Simple_command (Template.map ~f toks)
  | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
  | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
  | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))
