open Core.Std

let tex : [`latex] Bistro_file.t = Bistro_workflow.input "src/test/doc.tex"

let fig : [`pdf] Bistro_file.t = Bistro_workflow.(
  let script = L [
    S"set terminal pdf ; " ;
    S"set output \"" ; D ; S"\" ; " ;
    S"plot cos(x)"
  ]
  in
  make [
    L [S"gnuplot" ; S"-e" ; Q script]
  ])

let pdf : [`pdf] Bistro_file.t = Bistro_workflow.(
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

let () = Bistro_export.to_script (Bistro_db.make "_bistro") pdf stdout
