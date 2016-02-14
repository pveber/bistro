open Core.Std
open Lwt
open Bistro.Std
open Bistro_engine

module Workflow = Bistro.Workflow

type txt = ([`txt],[`text]) file

let a : txt workflow = Workflow.make [%sh{|
date > {{DEST}}
sleep 3
|}]

let b : txt workflow = Workflow.make [%sh{|
echo This is b > {{DEST}}
cat {{dep a}} >> {{DEST}}
date >> {{DEST}}
sleep 2
|}]

let c : txt workflow = Workflow.make ~descr:"C" [%sh{|
echo This is c > {{DEST}}
cat {{dep a}} >> {{DEST}}
date >> {{DEST}}
sleep 1
|}]

let d : txt workflow = Workflow.make ~descr:"D" [%sh{|
cat {{dep b}} {{dep c}} > {{DEST}}
|}]

let main db e =
  Scheduler.build_exn e d >>= fun s ->
  print_endline "Contents of a:" ;
  print_endline (In_channel.read_all (Db.workflow_path db a)) ;
  print_newline () ;
  print_endline "Contents of b:" ;
  print_endline (In_channel.read_all (Db.workflow_path db b)) ;
  print_newline () ;
  print_endline "Contents of c:" ;
  print_endline (In_channel.read_all (Db.workflow_path db c)) ;
  print_newline () ;
  print_endline "Contents of d:" ;
  print_endline (In_channel.read_all (Db.workflow_path db d)) ;
  Lwt.return ()

let () = Lwt_unix.run (
    Db.with_open_exn "_bistro" (fun db ->
        let e = Scheduler.(make (local_backend ~np:2 ~mem:1024) db) in
        main db e
      )
  )
