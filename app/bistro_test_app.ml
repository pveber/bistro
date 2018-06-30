open Core
open Bistro_test

let () =
  Command.group ~summary:"Bistro test app" [
    "accordion",  Accordion.command ;
    "ppx",        Ppx_test.command ;
    "prime-tdag", Prime_tdag.command ;
  ]
  |> Command.run

