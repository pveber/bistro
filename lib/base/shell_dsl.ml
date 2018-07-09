open Core_kernel

include Template_dsl

type command = Workflow.shell_command

let docker image cmd = Command.Docker (image, cmd)

let gen_cmd prog_expr ?env ?stdin ?stdout ?stderr args =
  let stdout_expr =
    match stdout with
    | None -> []
    | Some e -> Template.S " > " :: e
  in
  let stdin_expr =
    match stdin with
    | None -> []
    | Some e -> Template.S " < " :: e
  in
  let stderr_expr =
    match stderr with
    | None -> []
    | Some e -> Template.S " 2> " :: e
  in
  let tokens =
    [ prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
    |> List.filter ~f:(( <> ) [])
    |> List.intersperse ~sep:(string " ")
    |> List.concat
  in
  let cmd = Command.Simple_command tokens in
  match env with
  | None -> cmd
  | Some image -> docker image cmd

let cmd p = gen_cmd [ S p ]

let opt o f x = Template.(S o :: S " " :: f x)

let opt' o f x = Template.(S o :: S "=" :: f x)

let flag f x b = if b then f x else []

let mkdir d = cmd "mkdir" [ d ]

let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

let cd p = cmd "cd" [ p ]

let rm_rf x = cmd "rm" [ string "-rf" ; x ]

let mv x y = cmd "mv" [ x ; y ]

let ( // ) x y = Template.(x @ [ S "/" ; S y ])

let or_list xs = Command.Or_list xs
let and_list xs = Command.And_list xs
let pipe xs = Command.Pipe_list xs


let ( % ) f g x = g (f x)

let docker_image = Command.docker_image