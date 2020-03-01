open Core_kernel
open Bistro_internals
open Lwt.Infix

let tag = function
  | None -> ""
  | Some t -> ":" ^ t

let url_of_image : Command.container_image -> string = function
  | Docker_image img ->
    sprintf "docker://%s/%s%s"
      img.account img.name
      (tag img.tag)
  | Singularity_image img ->
    sprintf "shub://%s/%s%s"
      img.account img.name
      (tag img.tag)

let fetch_image img dest =
  let url = url_of_image img in
  let cmd = [| "singularity" ; "pull" ; "--name" ; dest ; url |] in
  Lwt_process.exec ~stderr:`Dev_null ~stdout:`Dev_null ("", cmd) >>= function
  | WEXITED 0 -> Lwt_result.return ()
  | WEXITED n
  | WSIGNALED n
  | WSTOPPED n -> Lwt_result.fail (`Singularity_failed_pull (n, url))
