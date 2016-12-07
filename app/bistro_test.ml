open Core.Std

let () =
  Command.group ~summary:"Bistro test app" [
    "accordion", Accordion.command ;
    "prime-tdag", Prime_tdag.command ;
  ]
  |> Command.run

