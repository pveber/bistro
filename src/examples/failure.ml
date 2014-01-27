open Bistro_types
open Core.Std

let bar : unit workflow = Bistro_workflow.make <:script<
  touch #{DEST}
>>

let foo (x : unit workflow) : unit workflow = Bistro_workflow.make <:script<
  echo #w:x# > #{DEST}
>>

let fail x = Bistro_workflow.make <:script<
  exit 1
  echo #w:x# > #{DEST}
>>

let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

let goal = foo (fail bar)

let () =
  Bistro_sequential.exec db logger goal
