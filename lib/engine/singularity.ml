open Core_kernel
open Bistro_internals
open Lwt.Infix

let url_of_image : Command.container_image -> string = function
  | Docker_image img ->
    sprintf "docker://%s/%s%s"
      img.account img.name
      (Option.value ~default:"" img.tag)
  | Singularity_image img ->
    sprintf "shub://%s/%s%s"
      img.account img.name
      (Option.value ~default:"" img.tag)

let fetch_image db img =
  let url = url_of_image img in
  let cmd = [|
    "singularity" ; "pull" ;
    (Db.singularity_image db img) ;
    url
  |]
  in
  Lwt_process.exec ("", cmd) >>= function
  | WEXITED 0 -> Lwt_result.return ()
  | WEXITED n
  | WSIGNALED n
  | WSTOPPED n -> Lwt_result.fail (`Singularity_failed_pull (n, url))
