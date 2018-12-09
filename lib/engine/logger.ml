open Bistro_internals

type time = float

type event =
  | Workflow_ready : _ Workflow.t -> event
  | Workflow_started : _ Workflow.t * Allocator.resource -> event
  | Workflow_ended : {
      outcome : Task_result.t ;
      start : time ;
      _end_ : time ;
    } -> event
  | Workflow_skipped : _ Workflow.t * [ `Done_already | `Missing_dep ] -> event
  | Workflow_allocation_error : _ Workflow.t * string -> event
  | Workflow_collected : _ Workflow.t -> event

class type t = object
  method event : Db.t -> time -> event -> unit
  method stop : unit Lwt.t
end

let null = object
  method event _ _ _ = ()
  method stop = Lwt.return ()
end

let tee loggers = object
  method event x y z =
    List.iter (fun l -> l#event x y z) loggers

  method stop =
    Lwt.join (List.map (fun l -> l#stop) loggers)
end
