open Bistro

let f () = [%workflow assert false]

let g = Workflow.trywith (f ()) (Workflow.data 42)

let h i = [%workflow
  string_of_int [%eval i]
]

module Top = Bistro_utils.Toplevel_eval.Make(struct
    let np = 1
    let mem = 4 * 1024
  end)()

let () =
  Top.eval (h g)
  |> print_endline
