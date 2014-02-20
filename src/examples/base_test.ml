open Core.Std
open Bistro_types

let tex : [`latex] file workflow = Bistro_workflow.input "src/examples/doc.tex"

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

let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

let _ = React.E.trace print_endline (Bistro_logger.to_strings logger)

let () = Bistro_sequential.exec db logger pdf
