open Core.Std
open Bistro

let tex : [`latex] File.t = Workflow.input "src/test/doc.tex"

let fig : [`pdf] File.t = Workflow.(
  let script = S [
    A"set terminal pdf ; " ;
    A"set output \"" ; D ; A"\" ; " ;
    A"plot cos(x)"
  ]
  in
  make [
    S [A"gnuplot" ; A"-e" ; Q script]
  ])

let pdf : [`pdf] File.t = Workflow.(
  let temp_dir = Q (S [ D ; A"_tmp" ]) in
  make [
    S [A"mkdir" ; A"-p" ; Q temp_dir] ;
    S [A"cp" ; W tex ; Q (S [temp_dir ; A"/doc.tex"])] ;
    S [A"cp" ; W fig ; Q (S [temp_dir ; A"/fig.pdf"])] ;
    S [A"(" ; A"cd" ; temp_dir ; A"&&" ; A"pdflatex" ; A"doc" ;A")"] ;
    S [A"mv" ; Q (S [temp_dir ; A"/doc.pdf"]) ; D] ;
    S [A"rm" ; A"-rf" ; temp_dir ]
  ]
  |> depends ~on:fig (* fig is added as a new dep although it is useless *)
)

let () = Export.to_script ~cache_dir:"_bistro" pdf stdout
