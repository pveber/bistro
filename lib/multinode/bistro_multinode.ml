open Core_kernel
open Bistro_engine
open Lwt.Infix

type job =
  | Plugin of {
      workflow_id : string ;
      f : unit -> unit ;
    }
  | Shell_command of {
      workflow_id : string ;
      cmd : Shell_command.t ;
    }

type client_id = Client_id of string

type _ api_request =
  | Subscript : { np : int ; mem : int } -> client_id api_request
  | Get_job : { client_id : string } -> job option api_request
  | Plugin_result : {
      client_id : string ;
      workflow_id : string ;
      result : (unit, string) Result.t ;
    } -> unit api_request
  | Shell_command_result : {
      client_id : string ;
      workflow_id : string ;
      result : int * bool ;
    } -> unit api_request

module Client = struct
  type t = {
    np : int ;
    mem : int ;
    hostname : string ;
    port : int ;
  }

  let with_connection { hostname ; port ; _ } ~f =
    Lwt_io.with_connection Unix.(ADDR_INET (inet_addr_of_string hostname, port)) f

  let send_request x (msg : 'a api_request) : 'a Lwt.t =
    with_connection x ~f:(fun (ic, oc) ->
        Lwt_io.write_value oc msg >>= fun () ->
        Lwt_io.flush oc >>= fun () ->
        Lwt_io.read_value ic
      )

  let main ~np ~mem ~hostname ~port () =
    let mem = mem * 1024 in
    let client = { np ; mem ; hostname ; port } in
    (* let alloc = Allocator.create ~np ~mem in *)
    let stop_var = Lwt_mvar.create_empty () in
    send_request client (Subscript { np ; mem }) >>= fun (Client_id client_id) ->
    printf "Received id: %s\n%!" client_id ;
    let job_thread = function
      | Plugin { workflow_id ; f } ->
        Local_backend.eval () () f () >>= fun result ->
        send_request client (Plugin_result { client_id ; workflow_id ; result })
      | Shell_command { workflow_id ; cmd } ->
        Shell_command.run cmd >>= fun result ->
        send_request client (Shell_command_result { client_id ; workflow_id ; result })
    in
    let rec loop () =
      Lwt.pick [
        (send_request client (Get_job { client_id }) >|= fun x -> `New_job x) ;
        Lwt_mvar.take stop_var >|= fun () -> `Stop
      ]
      >>= function
      | `New_job None
      | `Stop -> Lwt.return ()
      | `New_job (Some job) ->
        print_endline "Received new job!" ;
        Lwt.async (fun () -> job_thread job) ;
        loop ()
    in
    loop ()

  let command =
    let open Command.Let_syntax in
    Command.basic ~summary:"Bistro client" [%map_open
      let np = flag "--np" (required int) ~doc:"INT Number of available cores"
      and mem = flag "--mem" (required int) ~doc:"INT Available memory (in GB)"
      and hostname = flag "--hostname" (required string) ~doc:"ADDR Bistro server address"
      and port = flag "--port" (required int) ~doc:"INT Bistro server port"
      in
      fun () ->
        main ~np ~mem ~hostname ~port ()
        |> Lwt_main.run
    ]
end

module Server = struct
  module Backend = struct

    type job_waiter =
      | Waiting_shell_command of {
          workflow_id : string ;
          cmd : Shell_command.t ;
          waiter : (int * bool) Lwt.u ;
        }
      | Waiting_plugin of {
          workflow_id : string ;
          f : unit -> unit ;
          waiter : (unit, string) result Lwt.u ;
        }

    type worker = Worker of {
        id : string ;
        np : int ;
        mem : int ;
        mutable available_resource : Allocator.resource ;
        pending_jobs : job_waiter Lwt_queue.t ;
        running_jobs : job_waiter String.Table.t ;
      }

    module Worker_allocator = struct
      type t = {
        mutable available : Allocator.resource String.Table.t ;
        mutable waiters : ((int * int) * (string * Allocator.resource) Lwt.u) list ;
      }

      let create () = {
        available = String.Table.create () ;
        waiters = [] ;
      }

      let search (type s) (table : s String.Table.t) ~f =
        let module M = struct exception Found of string * s end in
        try
          String.Table.fold table ~init:() ~f:(fun ~key ~data () -> if f ~key ~data then raise (M.Found (key, data))) ;
          None
        with M.Found (k, v) -> Some (k, v)

      let allocation_pass pool =
        let remaining_waiters =
          List.filter_map pool.waiters ~f:(fun ((np, mem), u as elt) ->
              let allocation_attempt =
                search pool.available ~f:(fun ~key:_ ~data:(Resource curr) ->
                    curr.np >= np && curr.mem >= mem
                  )
              in
              match allocation_attempt with
              | None -> Some elt
              | Some (worker_id, (Resource curr)) ->
                String.Table.set pool.available ~key:worker_id ~data:(Resource { np = curr.np - np ; mem = curr.mem - mem }) ;
                Lwt.wakeup u (worker_id, Resource { np ; mem }) ;
                None
            )
        in
        pool.waiters <- remaining_waiters

      let request pool (Allocator.Request { np ; mem }) =
        let t, u = Lwt.wait () in
        let waiters =
          ((np, mem), u) :: pool.waiters
          |> List.sort ~compare:(fun (x, _) (y,_) -> compare y x)
        in
        pool.waiters <- waiters ;
        allocation_pass pool ;
        t

      let add_worker pool (Worker { id ; np ; mem ; _ }) =
        match String.Table.add pool.available ~key:id ~data:(Allocator.Resource { np ; mem }) with
        | `Ok -> allocation_pass pool
        | `Duplicate -> failwith "A worker has been added twice"

      let release pool worker_id (Allocator.Resource { np ; mem }) =
        String.Table.update pool.available worker_id ~f:(function
            | None -> failwith "Tried to release resources of inexistent worker"
            | Some (Resource r) -> Resource { np = r.np + np ; mem = r.mem + mem }
          )
    end

    type token = {
      worker_id : string ;
      workflow_id : string ;
    }

    type state = {
      workers : worker String.Table.t ;
      alloc : Worker_allocator.t ;
    }

    type event = [
      | `Stop
      | `New_worker
    ]

    type t = {
      server : Lwt_io.server ;
      state : state ;
      events : event Lwt_react.event ;
      send_event : event -> unit ;
      stop_signal : unit Lwt_condition.t ;
      server_stop : unit Lwt.t ;
      logger : Logger.t ;
      db : Db.t ;
    }

    let new_id =
      let c = ref 0 in
      fun () -> incr c ; sprintf "w%d" !c

    let workflow_id_of_job_waiter = function
      | Waiting_plugin wp -> wp.workflow_id
      | Waiting_shell_command wsc -> wsc.workflow_id

    let job_of_job_waiter = function
      | Waiting_plugin { f ; workflow_id ; _ } ->
        Plugin { f ; workflow_id }
      | Waiting_shell_command { cmd ; workflow_id ; _ } ->
        Shell_command { cmd ; workflow_id }

    let create_worker ~np ~mem id =
      Worker {
        id ; np ; mem ;
        available_resource = Allocator.Resource { np ; mem } ;
        pending_jobs = Lwt_queue.create () ;
        running_jobs = String.Table.create () ;
      }

    let create_state () = {
      workers = String.Table.create () ;
      alloc = Worker_allocator.create () ;
    }

    let server_api
      : type s. (Logger.event -> unit) -> stop_signal:unit Lwt_condition.t -> state -> s api_request -> s Lwt.t
      = fun log ~stop_signal state msg ->
      match msg with

      | Subscript { np ; mem } ->
        let id = new_id () in
        let w = create_worker ~np ~mem id in
        String.Table.set state.workers ~key:id ~data:w ;
        Worker_allocator.add_worker state.alloc w ;
        log (Logger.Debug (sprintf "new worker %s" id)) ;
        Lwt.return (Client_id id)

      | Get_job { client_id } -> (
          match String.Table.find state.workers client_id with
          | None -> Lwt.return None
          | Some (Worker worker) ->
            Lwt.choose [
              (Lwt_queue.pop worker.pending_jobs >|= fun x -> `Job x) ;
              (Lwt_condition.wait stop_signal >|= fun () -> `Stop) ;
            ] >>= function
            | `Job wp ->
              let workflow_id = workflow_id_of_job_waiter wp in
              String.Table.set worker.running_jobs ~key:workflow_id ~data:wp ;
              Lwt.return (Some (job_of_job_waiter wp))
            | `Stop -> Lwt.return None
        )

      | Plugin_result r ->
        let Worker worker = String.Table.find_exn state.workers r.client_id in
        Lwt.return (
          match String.Table.find_exn worker.running_jobs r.workflow_id with
          | Waiting_plugin wp -> Lwt.wakeup wp.waiter r.result
          | Waiting_shell_command _ -> assert false
        )
      | Shell_command_result r ->
        let Worker worker = String.Table.find_exn state.workers r.client_id in
        Lwt.return (
          match String.Table.find_exn worker.running_jobs r.workflow_id with
          | Waiting_plugin _ -> assert false
          | Waiting_shell_command wp -> Lwt.wakeup wp.waiter r.result
        )

    let server_handler log ~stop_signal state _ (ic, oc) =
      Lwt_io.read_value ic >>= fun msg ->
      server_api log ~stop_signal state msg >>= fun res ->
      Lwt_io.write_value oc res ~flags:[Closures] >>= fun () ->
      Lwt_io.flush oc >>= fun () ->
      Lwt_io.close ic >>= fun () ->
      Lwt_io.close oc

    let create ?(loggers = []) ~port db =
      Lwt_unix.gethostname () >>= fun hostname ->
      Lwt_unix.gethostbyname hostname >>= fun h ->
      let sockaddr = Unix.ADDR_INET (h.Unix.h_addr_list.(0), port) in
      let state = create_state () in
      let logger = Logger.tee loggers in
      let log event = logger#event db (Unix.gettimeofday ()) event in
      let stop_signal = Lwt_condition.create () in
      Lwt_io.establish_server_with_client_address sockaddr (server_handler log ~stop_signal state) >>= fun server ->
      let events, send_event = Lwt_react.E.create () in
      let server_stop =
        Lwt_condition.wait stop_signal >>= fun () -> Lwt_io.shutdown_server server
      in
      Lwt.return {
        events ;
        send_event ;
        stop_signal ;
        server_stop ;
        server ;
        state ;
        logger = Logger.tee loggers ;
        db ;
      }

    let log ?(time = Unix.gettimeofday ()) backend event =
      backend.logger#event backend.db time event

    let request_resource backend req =
      Worker_allocator.request backend.state.alloc req >|= fun (worker_id, resource) ->
      String.Table.find_exn backend.state.workers worker_id, resource

    let release_resource backend worker_id res =
      Worker_allocator.release backend.state.alloc worker_id res

    let build_trace backend w requirement perform =
      let ready = Unix.gettimeofday () in
      log ~time:ready backend (Logger.Workflow_ready w) ;
      printf "Try to build %s\n%!" (Bistro_internals.Workflow.id w) ;
      request_resource backend requirement >>= fun (Worker worker, resource) ->
      let open Eval_thread.Infix in
      let start = Unix.gettimeofday () in
      log ~time:start backend (Logger.Workflow_started (w, resource)) ;
      let token = { worker_id = worker.id ; workflow_id = Bistro_internals.Workflow.id w } in
      perform token resource >>= fun outcome ->
      let _end_ = Unix.gettimeofday () in
      log ~time:_end_ backend (Logger.Workflow_ended { outcome ; start ; _end_ }) ;
      release_resource backend worker.id resource ;
      Eval_thread.return (
        Execution_trace.Run { ready ; start  ; _end_ ; outcome }
      )
        (* | Error `Resource_unavailable ->
         *   let msg = "No worker with enough resource" in
         *   log backend (Logger.Workflow_allocation_error (w, msg)) ;
         *   wait_for_new_worker backend >>= fun () ->
         *   loop () *)

    let eval backend { worker_id ; workflow_id } f x =
      let Worker worker = String.Table.find_exn backend.state.workers worker_id in
      let f () = f x in
      let t, u = Lwt.wait () in
      let job_waiter = Waiting_plugin { waiter = u ; f ; workflow_id } in
      Lwt_queue.push worker.pending_jobs job_waiter ;
      t

    let run_shell_command backend { worker_id ; workflow_id } cmd =
      let Worker worker = String.Table.find_exn backend.state.workers worker_id in
      let t, u = Lwt.wait () in
      let job = Waiting_shell_command { waiter = u ; cmd ; workflow_id } in
      Lwt_queue.push worker.pending_jobs job ;
      t

    let stop backend =
      Lwt_condition.broadcast backend.stop_signal () ;
      Lwt.return ()
  end

  module Scheduler = Scheduler.Make(Backend)

  type t = Scheduler.t

  let create ?allowed_containers ?loggers ?collect ?(port = 6666) db =
    Backend.create ?loggers ~port db >|= fun backend ->
    Scheduler.create ?allowed_containers ?loggers ?collect backend db

  let start sched =
    Scheduler.start sched

  let stop sched =
    Scheduler.stop sched

  let eval sched w =
    Scheduler.eval sched w

  let simple_app ?allowed_containers ?loggers ?collect ?port ?(db = "_bistro") w =
    let t =
      create ?allowed_containers ?loggers ?collect ?port (Db.init_exn db) >>= fun server ->
      start server ;
      eval server w >|= (
        function
        | Ok _ -> ()
        | Error e ->
          print_endline @@ Scheduler.error_report server e
      ) >>= fun () ->
      stop server
    in
    Lwt_main.run t


  let simple_command ~summary w =
    let open Command.Let_syntax in
    Command.basic ~summary [%map_open
      let port = flag "--port" (required int) ~doc:"INT Port"
      and verbose = flag "--verbose" no_arg ~doc:" Display more info"
      in
      let loggers = if verbose then [ Bistro_utils.Console_logger.create () ] else [] in
      fun () -> simple_app ~port ~loggers w
    ]
end
