open Bistro_std
open Lwt

module Workflow = Bistro.Workflow
module Engine = Bistro.Engine
module Db = Bistro.Db

type failure

let fail : failure workflow = Workflow.make [%bistro.sh {|
false
|}]

let db = Db.init_exn "_bistro"
let e = Engine.make ~np:2 ~mem:1024 db

let main () =
  Engine.build_exn e fail >>= fun s ->
  Lwt.return ()

let () =
  try Lwt_unix.run (main ())
  with Failure s -> print_endline s
