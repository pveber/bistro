open Core
open Lwt
open Bistro.Std
open Bistro_engine
open Bistro.EDSL_sh

type txt = ([`txt],[`text]) file

let a : txt workflow = workflow ~timeout:1 [
    cmd "uname" ~stdout:dest [ string "-a" ]
  ]

let main db e =
  Scheduler.build_exn e a >>= fun s ->
  print_endline "Contents of a:" ;
  print_endline (In_channel.read_all (Db.workflow_path db a)) ;
  return ()

let main queue workdir () = Lwt_unix.run (
    let backend = Bistro_pbs.Backend.make ~workdir ~queue in
    let db = Db.init_exn "_bistro" in
    let e = Scheduler.(make backend db) in
    main db e
  )

let spec =
  let open Command.Spec in
  empty
  +> flag "--queue" (required string) ~doc:"Name of a PBS queue"
  +> flag "--workdir" (required string) ~doc:"Node-local directory"

let command =
  Command.basic
    ~summary:"Runs test workflow on a PBS cluster"
    spec
    main

let () = Command.run ~version:"0.0" command
