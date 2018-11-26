open Core
open Bistro

let echo3 x = Workflow.makep [%bistro
    fun dest ->
      let x = [%eval x] in
      Out_channel.write_lines dest [ x ; x ; x ]
  ]

let wc x = Workflow.make [%bistro
    In_channel.read_lines [%path x]
    |> List.length
  ]

let request x =
  Workflow.make [%bistro String.split ~on:' ' x ]

let main () =
  request "am stram gram"
  |> Expr.eval_workflow
  |> Expr.spawn ~f:(fun x ->
      echo3 x
      |> wc
      |> Expr.eval_workflow
    )


module type API = sig
  type fasta

  val db_request : string -> string list workflow
  val fetch_sequences : org:string expr -> fasta path workflow
  val concat_fasta : fasta path list expr -> fasta path workflow
end
