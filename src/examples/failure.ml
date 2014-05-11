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
let logger = Bistro_logger.make ()

let goal = foo (fail bar)

let () =
  Bistro_run.exec db logger goal
