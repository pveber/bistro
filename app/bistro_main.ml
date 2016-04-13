open Core.Std

let main tmpdir outdir np mem fn () =
  let plan = In_channel.with_file fn ~f:Bistro_app.plan_of_channel in
  Bistro_app.local ?tmpdir ~outdir ~np ~mem:(mem * 1024) plan

let spec =
  let open Command.Spec in
  empty
  +> flag "--tmpdir"  (optional string) ~doc:"DIR (Preferably local) directory where to put temporary files"
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Runs a bistro workflow"
    spec
    main

let () = Command.run ~version:"0.1" command
