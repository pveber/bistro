open Core.Std
open Bistro

let tex = Workflow.input "doc.tex"

let fig = Workflow.(
  let script = [
    A"set terminal pdf ;" ;
    A"set output" ; D ; A";" ;
    A"plot cos(x)"
  ]
  in
  make [
    A"gnuplot" ; A"-e" ; Q script
  ])

let pdf = Workflow.(
  make [ A"pdflatex" ; W tex ]
  |> depends_on fig
)
