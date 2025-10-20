open Core

let w i =
  let f = fun%workflow () ->
    [%param i] + 1
  in
  Bistro.Workflow.plugin f

let command = Command.group ~summary:"Bistro multinode test application" [
    "server", Bistro_multinode.Server.simple_command ~summary:"Test server" (w 0) ;
    "client", Bistro_multinode.Client.command ;
  ]

let () = Command_unix.run command
