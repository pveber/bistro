open Core.Std

let () =
  Command.group ~summary:"Bistro test app" [
    "prime-tdag", Prime_tdag.command ;
  ]
  |> Command.run

