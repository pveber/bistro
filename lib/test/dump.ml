open Core.Std
open Bistro.Std
open Bistro.EDSL

let dump : [`txt] workflow =
  workflow ~descr:"dump_workflow" [
    dump ~dest (
      [ "a" ; "$PATH" ; "c" ]
      |> List.map ~f:string
      |> seq ~sep:"\n"
    )
  ]

let () =
  Bistro_app.(
    local ~outdir:"res" [
      [ "dump" ] %> dump ;
    ]
  )
