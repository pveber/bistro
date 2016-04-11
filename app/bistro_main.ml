open Core.Std

let main workdir np mem fn () =
  let plan = Bistro_app.load_plan fn in
  Bistro_app.local ?workdir ~np ~mem:(mem * 1024) plan

let spec =
  let open Command.Spec in
  empty
  +> flag "--workdir" (optional string) ~doc:"DIR (Preferably local) directory where to put temporary files"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Runs a bistro workflow"
    spec
    main

let () = Command.run ~version:"0.1" command
