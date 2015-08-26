open Core.Std
open Lwt
open Bistro_std

type txt = ([`txt],[`text]) file

let a : txt workflow = Workflow.make [%bistro.sh {|
date > {{DEST}}
sleep 2
|}]

let b : txt workflow = Workflow.make [%bistro.sh {|
echo This is b > {{DEST}}
cat {{dep a}} > {{DEST}}
date >> {{DEST}}
|}]

let c : txt workflow = Workflow.make [%bistro.sh {|
echo This is c > {{DEST}}
cat {{dep a}} > {{DEST}}
date >> {{DEST}}
|}]

let d : txt workflow = Workflow.make [%bistro.sh {|
cat {{dep b}} {{dep c}} > {{DEST}}
|}]

let db = Db.init_exn "_bistro"
let e = Engine.make ~np:1 ~mem:1024 db

let main () =
  Engine.build_exn e d >>= fun s ->
  print_endline (In_channel.read_all s) ;
  Lwt.return ()

let () = Lwt_unix.run (main ())
