open Core

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

let quote s = sprintf "'%s'" s

open Lwt

let exec_exn cmd =
  Lwt_process.exec ("", cmd) >>= function
  | WEXITED 0 -> Lwt.return ()
  | _ -> Lwt.fail_with (String.concat ~sep:" " @@ Array.to_list cmd)

let mv src dst =
  exec_exn [| "mv" ; src ; dst |]

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    exec_exn [| "rm" ; "-rf" ; fn |]
  else
    Lwt.return ()

let redirection filename =
  let flags = Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) in
  Lwt_unix.openfile filename flags 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let touch dst =
  exec_exn [| "touch" ; dst |]

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
    "ln" ; "-s" ; absolutize from ; absolutize _to_ ;
  |]
  in
  exec_exn cmd

let cp from _to_ =
  let cmd = [|
    "cp" ; "-r" ; absolutize from ; absolutize _to_ ;
  |]
  in
  exec_exn cmd

let files_in_dir dir =
  Lwt_unix.files_of_directory dir
  |> Lwt_stream.to_list >|=
  List.filter ~f:(function
      | "." | ".." -> false
      | _ -> true
    )

let files_in_dir_blocking dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter ~f:(function
      | "." | ".." -> false
      | _ -> true
    )
