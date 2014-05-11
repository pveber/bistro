open Bistro_workflow.Types
open Core.Std

let bar : unit workflow = Bistro_workflow.make <:script<
  touch #DEST
>>

let foo (x : unit workflow) : unit workflow = Bistro_workflow.make <:script<
  echo #w:x# > #DEST
>>

let fail x = Bistro_workflow.make <:script<
  exit 1
  echo #w:x# > #DEST
>>

let db = Bistro_db.init "_bistro"
let blog = Bistro_log.make ~db ~hook:(fun x -> print_endline (Bistro_log.Entry.to_string x)) ()
let backend = Bistro_engine.local_worker blog

let goal = foo (fail bar)

let () = Bistro_engine.run db blog backend goal
