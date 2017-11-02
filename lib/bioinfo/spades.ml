open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"spades" ~tag:"3.9.1" ()

let renamings (ones, twos) =
  let f side i x =
    let id = sprintf "pe%d-%d" (i + 1) side in
    let new_name = seq ~sep:"/" [ tmp ; string (id ^ ".fq") ] in
    let opt = opt (sprintf "--pe%d-%d" (i + 1) side) ident new_name in
    let cmd = cmd "ln" [ string "-s" ; dep x ; new_name ] in
    opt, cmd
  in
  let r = List.mapi ones ~f:(f 1) @ List.mapi twos ~f:(f 2) in
  let args = seq ~sep:" " (List.map r ~f:fst) in
  let cmds = List.map r ~f:snd in
  Some args, cmds

let spades
    ?single_cell ?iontorrent
    ?pe
    ?(threads = 4)
    ?(memory = 10)
    ()
  : [`spades] directory workflow
  =
  let pe_args, ln_commands = match pe with
    | None -> None, []
    | Some files -> renamings files
  in
  workflow ~np:threads ~mem:(memory * 1024) ~descr:"spades" [
    mkdir_p tmp ;
    mkdir_p dest ;
    docker env (
      and_list (
        ln_commands @ [
          cmd "spades.py" ~env [
            option (flag string "--sc") single_cell ;
            option (flag string "--iontorrent") iontorrent ;
            opt "--threads" ident np ;
            opt "--memory" ident (seq [ string "$((" ; mem ; string " / 1024))" ]) ;
            option ident pe_args ;
            opt "-o" ident dest ;
          ]
        ]
      )
    )
  ]

let contigs = selector ["contigs.fasta"]
let scaffolds = selector ["scaffolds.fasta"]
