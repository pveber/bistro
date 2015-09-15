open Core.Std
open Lwt
open Bistro_std

module Workflow = Bistro.Workflow
module Engine = Bistro.Engine
module Db = Bistro.Db

type txt = ([`txt],[`text]) file

let a : txt workflow = Workflow.make [%bistro.sh {|
date > {{DEST}}
sleep 3
|}]

let b : txt workflow = Workflow.make [%bistro.sh {|
echo This is b > {{DEST}}
cat {{dep a}} >> {{DEST}}
date >> {{DEST}}
sleep 2
|}]

let c : txt workflow = Workflow.make [%bistro.sh {|
echo This is c > {{DEST}}
cat {{dep a}} >> {{DEST}}
date >> {{DEST}}
sleep 1
|}]

let d : txt workflow = Workflow.make [%bistro.sh {|
cat {{dep b}} {{dep c}} > {{DEST}}
|}]

let db = Db.init_exn "_bistro"
let e = Engine.make ~np:2 ~mem:1024 db

let main () =
  Engine.build_exn e d >>= fun s ->
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

let () = Lwt_unix.run (main ())
