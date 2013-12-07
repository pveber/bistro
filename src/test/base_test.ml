open Core.Std
open Bistro

let tex : [`latex] File.t = Workflow.input "src/test/doc.tex"

let fig : [`pdf] File.t = Workflow.(
  let script = L [
    S"set terminal pdf ; " ;
    S"set output \"" ; D ; S"\" ; " ;
    S"plot cos(x)"
  ]
  in
  make [
    L [S"gnuplot" ; S"-e" ; Q script]
  ])

let pdf : [`pdf] File.t = Workflow.(
  let temp_dir = Q (L [ D ; S"_tmp" ]) in
  make [
    L [S"mkdir" ; S"-p" ; Q temp_dir] ;
    L [S"cp" ; W tex ; Q (L [temp_dir ; S"/doc.tex"])] ;
    L [S"cp" ; W fig ; Q (L [temp_dir ; S"/fig.pdf"])] ;
    L [S"(" ; S"cd" ; temp_dir ; S"&&" ; S"pdflatex" ; S"doc" ;S")"] ;
    L [S"mv" ; Q (L [temp_dir ; S"/doc.pdf"]) ; D] ;
    L [S"rm" ; S"-rf" ; temp_dir ]
  ]
  |> depends ~on:fig (* fig is added as a new dep although it is useless *)
)

let () = Export.to_script ~cache_dir:"_bistro" pdf stdout
