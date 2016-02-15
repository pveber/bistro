open Core.Std
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bistro_engine

let db = Db.init_exn "_bistro"

let post_wave wave_name body =
  try
    let wave = Db.Wave.of_string body in
    Db.Wave_table.set db wave_name wave ;
    return (`OK, "")
  with
  | e -> return (`Bad_request, Exn.to_string e)

let path_of_uri uri =
  match String.split ~on:'/' (Uri.path uri) with
  | "" :: q -> q
  | p -> p

let callback _conn req body =
  let uri = Request.uri req in
  let meth = Request.meth req in
  Cohttp_lwt_body.to_string body >>= fun body ->
  (
    match meth, path_of_uri uri with
    | `POST, [ "wave" ; wave_name ] ->
      post_wave wave_name body
    | _ ->
      return (`Not_found, "Page not found!")
  )
  >>= fun (status, body) ->
  Server.respond_string ~status ~body ()

let server =
  Server.create
    ~mode:(`TCP (`Port 8000))
    (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
