let main ~program_path =
  In_channel.with_open_text program_path (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      match Parser.parse_program lexbuf with
      | Ok _ -> ()
      | Error (`Parser_error e) -> prerr_endline e.msg
    )

open Cmdliner

let cmd =
  let info = Cmd.info "run" in
  let term =
    let open Term.Syntax in
    let+ program_path = Arg.(
        let doc = "$(docv) is a path to the program to execute" in
        let docv = "PROG" in
        let info = info [] ~doc ~docv in
        required & pos 0 (some string) None info
    ) in
    main ~program_path
  in
  Cmd.v info term
