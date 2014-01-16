open Core.Std
open Bistro_types

let tex : [`latex] file workflow = Bistro_workflow.input "src/examples/doc.tex"

let fig : pdf workflow = Bistro_workflow.(
  make <:script<
    gnuplot -e 'set terminal pdf ; set output "%@" ; plot cos(x)'
  >>
)

let pdf : pdf workflow = Bistro_workflow.(
  make <:script<
    cp %tex% %@TMP/doc.tex
    cp %fig% %@TMP/fig.pdf
    (cd %@TMP && pdflatex doc)
    mv %@TMP/doc.pdf %@
  >>
  |> depends ~on:fig (* fig is added as a new dep although it is useless *)
)

(*
*)

let () = Bistro_export.to_script (Bistro_db.make "_bistro") pdf stdout
