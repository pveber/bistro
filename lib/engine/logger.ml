open Bistro_base

type time = float

type event =
  | Workflow_ready of Workflow.t
  | Workflow_started of Workflow.t * Allocator.resource
  | Workflow_ended of {
      outcome : Task_result.t ;
      start : time ;
      _end_ : time ;
    }
  | Workflow_skipped of Workflow.t * [ `Done_already | `Missing_dep ]
  | Workflow_allocation_error of Workflow.t * string

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

