open Bistro_base

type time = float

type event =
  | Task_ready of Task.t
  | Task_started of Task.t * Allocator.resource
  | Task_ended of {
      outcome : Task_result.t ;
      start : time ;
      _end_ : time ;
    }
  | Workflow_skipped of Workflow.u * [ `Done_already | `Missing_dep ]
  | Task_allocation_error of Task.t * string

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

