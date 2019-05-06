open Core
open Bistro

let w = Workflow.int 42

let command = Command.group ~summary:"Bistro multinode test application" [
    "server", Bistro_multinode.Server.simple_command ~summary:"Test server" w ;
    "client", Bistro_multinode.Client.command ;
  ]

let () = Command.run command
