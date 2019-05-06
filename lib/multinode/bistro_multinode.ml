open Core_kernel
open Bistro_engine
open Lwt.Infix

type client_request =
  | Offer of { np : int ; mem : int }
  (* | Im_stopping *)
[@@deriving sexp]

type server_answer =
  | Good_boy
  | Wait of float
  | Stop
[@@deriving sexp]

module Client = struct
  type t = {
    np : int ;
    mem : int ;
    hostname : string ;
    port : int ;
  }

  let with_connection { hostname ; port ; _ } ~f =
    Lwt_io.with_connection Unix.(ADDR_INET (inet_addr_of_string hostname, port)) f

  let send_msg x msg : server_answer Lwt.t =
    with_connection x ~f:(fun (ic, oc) ->
        Lwt_io.write_value oc (msg : client_request) >>= fun () ->
        Lwt_io.flush oc >>= fun () ->
        Lwt_io.read_value ic
      )

  let main ~np ~mem ~hostname ~port () =
    let client = { np ; mem ; hostname ; port } in
    let loop () =
      send_msg client (Offer { np = 42 ; mem = 42 }) >|= fun x ->
      if x = Good_boy then print_endline "I'm a good boy!"
    in    
    Lwt_main.run (loop ())

  let command =
    let open Command.Let_syntax in
    Command.basic ~summary:"Bistro client" [%map_open
      let np = flag "--np" (required int) ~doc:"INT Number of available cores"
      and mem = flag "--mem" (required int) ~doc:"INT Available memory (in GB)"
      and hostname = flag "--hostname" (required string) ~doc:"ADDR Bistro server address"
      and port = flag "--port" (required int) ~doc:"INT Bistro server port"
      in
      main ~np ~mem ~hostname ~port
    ]
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
      stop_signal : unit Lwt_condition.t ;
      server_stop : unit Lwt.t ;
    }

    let server_handler _ (ic, oc) =
      Lwt_io.read_value ic >>= fun msg ->
      (
        match msg with
        | Offer { np ; mem } -> printf "%d %d\n%!" np mem
      ) ;
      Lwt_io.write_value oc Good_boy >>= fun () ->
      Lwt_io.flush oc >>= fun () ->
      Lwt_io.close ic >>= fun () ->
      Lwt_io.close oc

    let create ~port =
      Lwt_unix.gethostname () >>= fun hostname ->
      Lwt_unix.gethostbyname hostname >>= fun h ->
      let sockaddr = Unix.ADDR_INET (h.Unix.h_addr_list.(0), port) in
      Lwt_io.establish_server_with_client_address sockaddr server_handler >>= fun server ->
      let stop_signal = Lwt_condition.create () in
      let server_stop =
        Lwt_condition.wait stop_signal >>= fun () -> Lwt_io.shutdown_server server
      in
      Lwt.return {
        stop_signal : unit Lwt_condition.t ;
        server_stop : unit Lwt.t ;
        server ;
        workers = [] ;
      }
    
    let build_trace _ = assert false
    let eval _ = assert false
    let run_shell_command _ = assert false
  end

  module Scheduler = Scheduler.Make(Backend)

  type t = Scheduler.t

  let create ?allowed_containers ?loggers ?collect ?(port = 6666) db =
    Backend.create ~port >|= fun backend ->
    Scheduler.create ?allowed_containers ?loggers ?collect backend db

  let start sched =
    Scheduler.start sched

  let stop sched =
    Scheduler.stop sched

  let eval sched w =
    Scheduler.eval sched w

  let simple_app ?allowed_containers ?loggers ?collect ?port ?(db = "_bistro") _w =
    let t =
      create ?allowed_containers ?loggers ?collect ?port (Db.init_exn db) >>= fun server ->
      start server ;
      Lwt_unix.sleep 30.
      (* FOR LATER *)
      (* eval server w >|= function
       * | Ok _ -> ()
       * | Error e ->
       *   print_endline @@ Scheduler.error_report server e *)
        
    in
    Lwt_main.run t
    

  let simple_command ~summary w =
    let open Command.Let_syntax in
    Command.basic ~summary [%map_open
      let port = flag "--port" (required int) ~doc:"INT Port"
      in
      fun () -> simple_app ~port w
    ]
end
