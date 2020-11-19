open Bistro
open Bistro.Shell_dsl

type t = fasta file

let arg = function
  | `plain fa -> dep fa
  | `gziped fa_gz -> Bistro_unix.Cmd.gzdep fa_gz

let concat xs =
  Workflow.shell ~descr:"fasta.concat" [
      cmd "cat" ~stdout:dest [
          list ~sep:" " arg xs ;
        ]
    ]

let concat_gz xs =
  Workflow.shell ~descr:"fasta.concat_gz" [
      cmd "cat" ~stdout:Bistro_unix.Cmd.gzdest [
          list ~sep:" " arg xs ;
        ]
    ]
