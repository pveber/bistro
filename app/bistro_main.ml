open Cmdliner

let root_cmd =
  let doc = "A program to specify and run scientific pipelines" in
  let info = Cmd.info "bistro" ~doc in
  Cmd.group info [
    Bistro_lang.Run_main.cmd ;
  ]

let () = exit @@ Cmd.eval root_cmd
