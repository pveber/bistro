open Core_kernel
open Bistro_engine
open Lwt.Infix

module Client = struct
  let command = Command.basic ~summary:"Bistro client" (Command.Param.return ignore)
end

module Server = struct
  module Backend = struct
    (* type job =
     *   | Shell_command of {
     *       cmd : Shell_command.t ;
     *       waiter : (int * bool) Lwt.u ;
     *     }
     *   | Eval of {
     *       f : unit -> unit ;
     *       waiter : (unit, string) result Lwt.u ;
     *     } *)

    type token = string
    type t = {
      server : Lwt_io.server ;
      workers : string list ;
    }

    let server_handler _ (ic, oc) =
      Lwt_io.close ic >>= fun () ->
      Lwt_io.close oc

    let create ~port =
      Lwt_unix.gethostname () >>= fun hostname ->
      Lwt_unix.gethostbyname hostname >>= fun h ->
      let sockaddr = Unix.ADDR_INET (h.Unix.h_addr_list.(0), port) in
      Lwt_io.establish_server_with_client_address sockaddr server_handler >>= fun server ->
      Lwt.return {
        server ;
        workers = [] ;
      }
    
    let build_trace _ = assert false
    let eval _ = assert false
    let run_shell_command _ = assert false
  end

  module Scheduler = Scheduler.Make(Backend)

  type t = {
    sched : Scheduler.t ;
    port : int ;
  }


  let create ?allowed_containers ?loggers ?collect ?(port = 6666) db =
    Backend.create ~port >>= fun backend ->
    let sched = Scheduler.create ?allowed_containers ?loggers ?collect backend db in
    Lwt.return { sched ; port }

  let start server =
    Scheduler.start server.sched

  let stop server =
    Scheduler.stop server.sched

  let eval server w =
    Scheduler.eval server.sched w

  let simple_app ?allowed_containers ?loggers ?collect ?port ?(db = "_bistro") w =
    let t =
      create ?allowed_containers ?loggers ?collect ?port (Db.init_exn db) >>= fun server ->
      start server ;
      eval server w
    in
    Lwt_main.run t

end
