open Core
open Bistro

let%bistro [@np 2] [@mem 1] [@descr "foobar"] [@version 42]
    comment_filter bed =
  In_channel.read_lines [%dep bed]
  |> List.filter ~f:(fun l -> not (String.is_prefix ~prefix:"#" l))
  |> Out_channel.write_lines [%dest]

let%bistro cat files =
  List.map [%deps files] ~f:(fun file ->
      In_channel.read_all file
    )
  |> String.concat ~sep:"\n"
  |> fun data -> Out_channel.write_all [%dest] ~data

let create_file contents =
  let file = string contents in
  workflow ~descr:"create_file" [
    cmd "cp" [ file_dump file ; dest ]
  ]

let%bistro test_id =
  let a = 1 in
  ignore a

let%bistro test_id' =
  let a = 1 in
  ignore a


let main () =
  assert Bistro.Workflow.(id test_id = id test_id') ;
  let bed = comment_filter (create_file "# comment\nchr1\t42\t100\n") in
  let bed2 = comment_filter (create_file "# comment\n# comment\nchr10\t42\t100\n") in
  Bistro.(
    match Workflow.u bed with
    | Step { descr ; version ; mem } ->
      assert (descr = "foobar") ;
      assert (version = Some 42) ;
      assert (mem = 1)
    | _ -> assert false
  ) ;
  Term.(
    run (
      pure (fun xs ->
          List.iter xs ~f:(fun (Path p) ->
              print_endline (In_channel.read_all p)
            )
        )
      $ list [ pureW bed ; pureW bed2 ]
    )
  )

let command =
  Command.basic
    ~summary:"Basic test of PPX extension"
    (Command.Param.return main)
