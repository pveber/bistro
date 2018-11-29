(* open Lwt.Infix *)
open Bistro_nlp

let pipeline w =
  wikipedia_summary w
  |> Stanford_parser.lexparser

let _ =
  Bistro_engine.Scheduler.eval (pipeline "Protein")
  (* >|= Printf.printf "%s\n" *)
  |> Lwt_main.run
