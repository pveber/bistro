open Core

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

open Lwt

let mv src dst =
  Lwt_process.exec ("", [| "mv" ; src ; dst |]) >|= ignore

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-rf" ; fn |]) >|= ignore
  else
    Lwt.return ()

let redirection filename =
  let flags = Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) in
  Lwt_unix.openfile filename flags 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let touch dst =
  Lwt_process.exec ("", [| "touch" ; dst |]) >|= ignore

let docker_chown ~path ~uid =
  let cmd = Docker.chown_command ~path ~uid in
  Lwt_process.(exec (shell cmd)) >|= ignore

let absolutize p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let relativize ~from p =
  let open Path in
  make_relative ~from p
  |> to_string

let ln from _to_ =
  let cmd = [|
    "ln" ; "-s" ; absolutize from ; absolutize _to_
  |]
  in
  Lwt_process.exec ("", cmd) >|= ignore

let files_in_dir dir =
  Lwt_unix.files_of_directory dir
  |> Lwt_stream.to_list >|=
  List.filter ~f:(function
      | "." | ".." -> false
      | _ -> true
    )
