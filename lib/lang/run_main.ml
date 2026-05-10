open Cmdliner

let cmd =
  let info = Cmd.info "run" in
  let term =
    Term.(app (const ignore) (const ()))
  in
  Cmd.v info term
