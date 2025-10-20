open Core
open Bistro

let echo3 ~sep x = Workflow.path_plugin ~descr:"echo3" (
    fun%workflow dest ->
      let x = [%eval x] in
      let sep = [%param sep] in
      Out_channel.write_lines dest [ x ; sep ; x ; sep ; x ]
  )

let wc x = Workflow.plugin ~descr:"wc" (
    fun%workflow () ->
      let x = [%path x] in
      In_channel.read_lines x
      |> List.length
  )

let request x =
  Workflow.plugin ~descr:"request" (
    fun%workflow () ->
      let x = [%param x] in
      String.split ~on:' ' x
  )

let main () =
  request "am stram gram"
  |> Workflow.spawn ~f:(fun x ->
      echo3 ~sep:"foo" x
      |> wc
    )
[@@ocaml.warning "-32"]

module type API = sig
  type fasta

  val db_request : string -> string list workflow
  val fetch_sequences : org:string workflow -> fasta path workflow
  val concat_fasta : fasta path list workflow -> fasta path workflow
end

module Pipeline(M : API) = struct
  open M

  let f req =
    db_request req
    |> Workflow.spawn ~f:(fun org -> fetch_sequences ~org)
    |> concat_fasta
end
[@@ocaml.warning "-32"]
