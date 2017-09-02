open Core
open Bistro.EDSL
open Bistro_utils

let comment_filter bed = [%bistro_fun
  let open Core_kernel in
  In_channel.read_lines [%dep bed]
  |> List.filter ~f:(fun l -> not (String.is_prefix ~prefix:"#" l))
  |> Out_channel.write_lines [%dest]
] ~descr:"comment_filter" ~np:1 ()

let create_file =
  let file = string "# comment\nchr1\t42\t100\n" in
  workflow ~descr:"create_file" [
    cmd "cp" [ file_dump file ; dest ]
  ]

let main () =
  Bistro_app.(
    run (
      pure (fun (Path p) -> print_endline (In_channel.read_all p))
      $ pureW (comment_filter create_file)
    )
  )

let command =
  Command.basic
    ~summary:"Basic test of PPX extension"
    Command.Spec.(empty)
    main
