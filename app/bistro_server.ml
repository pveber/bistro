open Core.Std
open Lwt
open Cohttp
open Cohttp_lwt_unix

let path_of_uri uri =
  match String.split ~on:'/' (Uri.path uri) with
  | "" :: q -> q
  | p -> p

let callback _conn req body =
  let uri = Request.uri req in
  let meth = Request.meth req in
  match meth, path_of_uri uri with
  | `POST, [ "wave" ] ->
    Server.respond_string ~status:`OK ~body:"" ()
  | _ ->
    Server.respond_string ~status:`Not_found ~body:"Page not found!" ()

let server =
  Server.create
    ~mode:(`TCP (`Port 8000))
    (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
