open Core.Std
open Bistro_workflow.Types

let tex : ([`latex], [`text]) file workflow = Bistro_workflow.input "src/examples/doc.tex"

let fig : pdf workflow = Bistro_workflow.(
  make <:script<
    gnuplot -e 'set terminal pdf ; set output "#DEST" ; plot cos(x)'
  >>
)

let pdf : pdf workflow = Bistro_workflow.(
  make <:script<
    cp #w:tex# #TMP/doc.tex
    cp #w:fig# #TMP/fig.pdf
    (cd #TMP && pdflatex doc)
    mv #TMP/doc.pdf #DEST
  >>
  |> depends ~on:fig (* fig is added as a new dep although it is useless *)
)

let db = Bistro_db.init "_bistro"
let blog = Bistro_log.make ~db ~hook:(fun x -> print_endline (Bistro_log.Entry.to_string x)) ()
let backend = Bistro_engine.local_worker blog

let () = Bistro_engine.run db blog backend pdf
