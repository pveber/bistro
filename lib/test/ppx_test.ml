open Core
open Bistro
open Shell_dsl

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
  shell ~descr:"create_file" [
    cmd "cp" [ file_dump file ; dest ]
  ]

let%bistro test_id =
  let a = 1 in
  ignore a

let%bistro test_id' =
  let a = 1 in
  ignore a

let%bistro test_param =
  let a = [%param 2] in
  ignore a

let%bistro test_param' =
  let a = [%param "foo"] in
  ignore a

let main () =
  let open Bistro_base in
  let open Bistro.Private in
  assert Workflow.(id (reveal test_id) = id (reveal test_id')) ;
  assert Workflow.(id (reveal test_param) <> id (reveal test_param')) ;
  let bed = comment_filter (create_file "# comment\nchr1\t42\t100\n") in
  let bed2 = comment_filter (create_file "# comment\n# comment\nchr10\t42\t100\n") in
  Bistro.(
    match Private.reveal bed with
    | Closure { descr ; version ; mem } ->
      assert (descr = "foobar") ;
      assert (version = Some 42) ;
      assert (mem = 1)
    | _ -> assert false
  ) ;
  eval_expr Expr.(
      pure ~id:"foobar" (fun xs ->
          List.iter xs ~f:(fun p ->
              print_endline (In_channel.read_all p)
            )
        )
      $ deps (list pureW [ bed ; bed2 ])
    )
  |> (
    function
      Ok () -> ()
    | Error msg -> prerr_endline msg
  )

let command =
  Command.basic
    ~summary:"Basic test of PPX extension"
    (Command.Param.return main)
