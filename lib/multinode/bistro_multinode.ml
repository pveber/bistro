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
    let client = { np ; mem ; hostname ; port } in
    (* let alloc = Allocator.create ~np ~mem in *)
    let stop_var = Lwt_mvar.create_empty () in
    send_request client (Subscript { np ; mem }) >>= fun (Client_id client_id) ->
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
        alloc : Allocator.t ;
        pending_jobs : job_waiter Lwt_queue.t ;
        running_jobs : job_waiter String.Table.t ;
      }

    type token = {
      worker_id : string ;
      workflow_id : string ;
    }

    type state = {
      workers : worker String.Table.t ;
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
        alloc = Allocator.create ~np ~mem ;
        pending_jobs = Lwt_queue.create () ;
        running_jobs = String.Table.create () ;
      }

    let create_state () = {
      workers = String.Table.create () ;
    }

    let server_api : type s. state -> s api_request -> s Lwt.t = fun state msg ->
      match msg with

      | Subscript { np ; mem } ->
        let id = new_id () in
        let w = create_worker ~np ~mem id in
        String.Table.set state.workers ~key:id ~data:w ;
        Lwt.return (Client_id id)

      | Get_job { client_id } -> (
          match String.Table.find state.workers client_id with
          | None -> Lwt.return None
          | Some (Worker worker) ->
            Lwt_queue.pop worker.pending_jobs >>= fun wp ->
            let workflow_id = workflow_id_of_job_waiter wp in
            String.Table.set worker.running_jobs ~key:workflow_id ~data:wp ;
            Lwt.return (Some (job_of_job_waiter wp))
        )

      | Plugin_result _ -> assert false

      | Shell_command_result _ -> assert false

    let server_handler state _ (ic, oc) =
      Lwt_io.read_value ic >>= fun msg ->
      server_api state msg >>= fun res ->
      Lwt_io.write_value oc res >>= fun () ->
      Lwt_io.flush oc >>= fun () ->
      Lwt_io.close ic >>= fun () ->
      Lwt_io.close oc

    let create ?(loggers = []) ~port db =
      Lwt_unix.gethostname () >>= fun hostname ->
      Lwt_unix.gethostbyname hostname >>= fun h ->
      let sockaddr = Unix.ADDR_INET (h.Unix.h_addr_list.(0), port) in
      let state = create_state () in
      Lwt_io.establish_server_with_client_address sockaddr (server_handler state) >>= fun server ->
      let events, send_event = Lwt_react.E.create () in
      let stop_signal = Lwt_condition.create () in
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
      let allocation_race =
        String.Table.to_alist backend.state.workers
        |> List.map ~f:(fun (_, (Worker { alloc ; _ } as w)) ->
            w,
            Allocator.request alloc req >|= fun r -> r, w
          )
      in
      let rec loop xs =
        if xs = [] then Lwt_result.fail `Resource_unavailable
        else
          Lwt.choose (List.map ~f:snd xs) >>= fun (r, (Worker w_first as worker_first)) ->
          let others = List.filter xs ~f:(fun (Worker w, _) -> w.id <> w_first.id) in
          match r with
          | Ok resource ->
            let cancellations =
              List.map others ~f:(fun (Worker w, t) ->
                  t >|= function
                  | Ok r, _ -> Allocator.release w.alloc r
                  | Error _, _ -> ()
                )
            in
            Lwt.async (fun () -> Lwt.join cancellations) ;
            Lwt_result.return (worker_first, resource)
          | Error _ -> loop others
      in
      loop allocation_race

    let rec wait_for_new_worker backend =
      Lwt_react.E.next backend.events >>= function
      | `New_worker -> Lwt.return ()
      | _ -> wait_for_new_worker backend

    let build_trace backend w requirement perform =
      let ready = Unix.gettimeofday () in
      log ~time:ready backend (Logger.Workflow_ready w) ;
      let rec loop () =
        request_resource backend requirement >>= function
        | Ok (Worker worker, resource) ->
          let open Eval_thread.Infix in
          let start = Unix.gettimeofday () in
          log ~time:start backend (Logger.Workflow_started (w, resource)) ;
          let token = { worker_id = worker.id ; workflow_id = Bistro_internals.Workflow.id w } in
          perform token resource >>= fun outcome ->
          let _end_ = Unix.gettimeofday () in
          log ~time:_end_ backend (Logger.Workflow_ended { outcome ; start ; _end_ }) ;
          Allocator.release worker.alloc resource ;
          Eval_thread.return (
            Execution_trace.Run { ready ; start  ; _end_ ; outcome }
          )
        | Error `Resource_unavailable ->
          let msg = "No worker with enough resource" in
          log backend (Logger.Workflow_allocation_error (w, msg)) ;
          wait_for_new_worker backend >>= fun () ->
          loop ()
      in
      loop ()

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
      eval server w >|= function
      | Ok _ -> ()
      | Error e ->
        print_endline @@ Scheduler.error_report server e

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
