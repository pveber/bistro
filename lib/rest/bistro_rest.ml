open Core.Std
open Rresult
open Bistro_engine
open Lwt
open Cohttp
open Cohttp_lwt_unix

type 'a result = ('a, R.msg) Rresult.result

type cfg = {
  host : string ;
  port : int
}

let make_uri { host ; port } path query =
  let path = String.concat ~sep:"/" path in
  Uri.make ~scheme:"http" ~host ~port ~path ~query ()

let post_wave cfg wave_name wave =
  let body =
    Db.Wave.to_string wave
    |> Cohttp_lwt_body.of_string
  in
  let uri = make_uri cfg [ "wave" ; wave_name ] [] in
  let () = print_endline (Uri.to_string uri) in
  Client.post ~body uri >>= fun (resp, body) ->
  match resp.Response.status with
  | `OK -> return (Ok ())
  | code ->
    Cohttp_lwt_body.to_string body >|= fun body ->
    R.error_msgf
      "post_wave failed with error %s and body %s"
      (Code.string_of_status code)
      body
