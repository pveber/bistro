open Core_kernel
open Bistro_engine

module Client = struct
  let command = Command.basic ~summary:"Bistro client" (Command.Param.return ignore)
end

module Server = struct
  type t = {
    sched : Scheduler.t ;
    port : int ;
  }

  let create ?np ?mem ?allowed_containers ?loggers ?collect ?(port = 6666) db =
    let sched = Scheduler.create ?np ?mem ?allowed_containers ?loggers ?collect db in
    { sched ; port }

  let start server =
    Scheduler.start server.sched

  let stop server =
    Scheduler.stop server.sched

  let eval server w =
    Scheduler.eval server.sched w

  let simple_app ?np ?mem ?allowed_containers ?loggers ?collect ?port ?(db = "_bistro") w =
    let server = create ?np ?mem ?allowed_containers ?loggers ?collect ?port (Db.init_exn db) in
    let r = eval server w in
    start server ;
    Lwt_main.run r
    
end
