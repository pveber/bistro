open Core
open Bistro.Std
open Bistro.EDSL
open Bistro_utils

let dir = workflow (
    mkdir_p dest
    :: List.init 20 ~f:(fun i ->
        let id = sprintf "f%d" i in
        cmd "echo" ~stdout:(dest // id) [ string id ]
      )
  )

let pwc =
  map_command ~np:4 dir (fun x ->
      and_list [
        cmd "sleep" [ int 1 ] ;
        cmd "wc" ~stdout:dest [ dep x ] ;
      ]
    )

let main () =
  let logger = Console_logger.create () in
  let repo = Repo.[ ["output"] %> pwc ] in
  Repo.build ~np:4 ~logger repo ~outdir:"out"
  |> ignore

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Test of map command workflows"
    (Command.Param.return main)
