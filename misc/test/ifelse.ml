open Bistro

let b () = [%workflow 42 mod 2 = 0]

let f () = Workflow.(ifelse (b ()) (data "even") (data "odd"))

module Top = Bistro_utils.Toplevel_eval.Make(struct
    let np = 1
    let mem = 4 * 1024
  end)()

let () =
  Top.eval (f ())
  |> print_endline
