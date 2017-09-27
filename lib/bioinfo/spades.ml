open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"spades" ~tag:"3.9.1" ()

let pe_args (ones, twos) =
  let opt side i x =
    opt (sprintf "--pe%d-%d" (i + 1) side) dep x
  in
  seq ~sep:" " (
    List.mapi ones ~f:(opt 1)
    @
    List.mapi twos ~f:(opt 2)
  )

let spades
    ?single_cell ?iontorrent
    ?pe
    ?(threads = 4)
    ?(memory = 10)
    ()
  : [`spades] directory workflow
  =
  workflow ~np:threads ~mem:(memory * 1024) ~descr:"spades" [
    mkdir_p dest ;
    cmd "spades.py" ~env [
      option (flag string "--sc") single_cell ;
      option (flag string "--iontorrent") iontorrent ;
      opt "--threads" ident np ;
      opt "--memory" (fun m -> seq [ string "$((" ; mem ; string " / 1024))" ]) mem ;
      option pe_args pe ;
      opt "-o" ident dest ;
    ]
  ]

let contigs = selector ["contigs.fasta"]
let scaffolds = selector ["scaffolds.fasta"]
