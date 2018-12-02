open Core_kernel
open Bistro
open Bistro_nlp
open Bistro_utils

let cut_deps x = [%workflow
  let lines = In_channel.read_lines [%path x] in
  List.group lines ~break:(fun _ l -> l = "")
  |> List.filter ~f:(( <> ) [""])
]

let%pworkflow dump_lines x =
  Out_channel.write_lines [%dest] [%eval x]

let pipeline w =
  wikipedia_summary w
  |> Stanford_parser.lexparser
  |> cut_deps
  |> Workflow.spawn ~f:(fun deps ->
      dump_lines deps
      |> Stanford_parser.dependensee
    )
  |> Repo.(items [ w ] ~prefix:"sentence" ~ext:"png")

let repo =
  [ "Protein" ; "Cell_(biology)" ]
  |> List.map ~f:pipeline
  
let () =
  Repo.build ~outdir:"res" repo
  |> Lwt_main.run
