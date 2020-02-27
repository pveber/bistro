open Core
open Bistro

let echo3 ~sep x = Workflow.path_plugin ~descr:"echo3" (
    let%pdeps x = x
    and     sep = data sep in
    Out_channel.write_lines __dest__ [ x ; sep ; x ; sep ; x ]
  )

let wc x = Workflow.plugin ~descr:"wc" (
    let%deps x = path x in
    In_channel.read_lines x
    |> List.length
  )

let request x =
  Workflow.plugin ~descr:"request" (
    let%deps x = data x in
    String.split ~on:' ' x
  )

let main () =
  request "am stram gram"
  |> Workflow.spawn ~f:(fun x ->
      echo3 ~sep:"foo" x
      |> wc
    )


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
