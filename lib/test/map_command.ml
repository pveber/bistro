open Core
open Bistro.Std
open Bistro.EDSL
open Bistro_utils

let dir = workflow [
    mkdir_p dest ;
    cmd "echo" ~stdout:(dest // "a") [ string "a" ] ;
    cmd "echo" ~stdout:(dest // "b") [ string "b" ] ;
  ]

let pwc =
  map_command dir (fun x -> cmd "wc" ~stdout:dest [ dep x ])

let main () =
  let logger = Console_logger.create () in
  let repo = Repo.[ ["output"] %> pwc ] in
  Repo.build ~logger repo ~outdir:"out"
  |> ignore

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Test of map command workflows"
    (Command.Param.return main)
