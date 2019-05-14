open Core

let%workflow w i =
  [%param i] + 1

let command = Command.group ~summary:"Bistro multinode test application" [
    "server", Bistro_multinode.Server.simple_command ~summary:"Test server" (w 0) ;
    "client", Bistro_multinode.Client.command ;
  ]

let () = Command.run command
