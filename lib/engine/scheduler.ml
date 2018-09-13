open Core_kernel
open Lwt.Infix
open Bistro_base

module S = String.Set

module Semaphore = struct
  type t = {
    mutable state : bool ;
    cond : unit Lwt_condition.t ;
  }

  let create state = {
    state ;
    cond = Lwt_condition.create () ;
  }

  let go s =
    s.state <- true ;
    Lwt_condition.broadcast s.cond ()

  (* let stop s =
   *   s.state <- false *)

  let wait s =
    if s.state then Lwt.return ()
    else Lwt_condition.wait s.cond
end

module Table = String.Table

module Lwt_eval = struct
  type 'a t = ('a, S.t) Lwt_result.t (* missing deps *)
  let ( >> ) = Lwt.( >>= )
  let ( >>= ) = Lwt_result.( >>= )
  (* let ( >|= ) = Lwt_result.( >|= ) *)

  let return = Lwt_result.return
  let fail = Lwt_result.fail
  (* let fail1 e = Lwt_result.fail (S.singleton e) *)

  let map_p xs ~f =
    Lwt.bind (Lwt_list.map_p f xs) @@ fun results ->
    let res =
      List.fold results ~init:(Ok []) ~f:(fun acc res ->
          match acc, res with
          | Ok xs, Ok x -> Ok (x :: xs)
          | Ok _, Error e -> Error e
          | Error e, Ok _ -> Error e
          | Error e, Error e' -> Error (S.union e e')
        )
      |> (
        function
        | Ok xs -> Ok (List.rev xs)
        | Error _ as e -> e
      )
    in
    Lwt.return res
end

type t = {
  allocator : Allocator.t ;
  config : Task.config ;
  traces : Execution_trace.t Lwt.t Table.t ;
  start_signal : Semaphore.t ;
  logger : Logger.t ;
}

let log ?(time = Unix.gettimeofday ()) sched event =
  sched.logger#event sched.config.db time event

let rec submit sched w : Execution_trace.t Lwt.t =
  match Table.find sched.traces (Workflow.id w) with
  | None ->
    let trace =
      Semaphore.wait sched.start_signal >>= fun () ->
      Task.is_done w sched.config.db >>= fun is_done ->
      if is_done then (
        log sched (Logger.Workflow_skipped (w, `Done_already)) ;
        Lwt.return Execution_trace.Done_already
      )
      else (
        compute_deps sched w >>= function
        | Ok _ ->
          task_trace sched (compute_task w)
        | Error missing_deps ->
          log sched Logger.(Workflow_skipped (w, `Missing_dep)) ;
          Lwt.return (Execution_trace.Canceled { missing_deps })
      )
    in
    Table.set sched.traces ~key:(Workflow.id w) ~data:trace ;
    trace
  | Some trace -> trace

and compute_deps sched w =
  let deps = Workflow.deps w in
  Lwt_eval.map_p ~f:(submit_for_eval sched) deps

and compute_task = function
  | Input { path ; id } ->
    Task.input ~path ~id
  | Select { dir ; sel ; _ } ->
    Task.select ~dir ~sel
  | Shell { task = cmd ; id ; descr ; np ; mem ; _ } ->
    Task.shell ~id ~descr ~np ~mem cmd
  | Plugin { task = f ; id ; descr ; np ; mem; _} ->
    Task.closure ~id ~descr ~np ~mem f

and submit_for_eval sched w =
  let open Lwt_eval in
  submit sched w >> fun trace ->
  if Execution_trace.is_errored trace then
    fail (Execution_trace.gather_failures [w] [trace])
  else
    return trace

and eval_expr : type s. t -> in_container:bool -> s Expr.t -> s Lwt_eval.t = fun sched ~in_container expr ->
  let open Expr in
  let open Lwt_eval in
  match expr with
  | Return x -> return x
  | Bind (x, f) ->
    eval_expr sched ~in_container x >>= fun x ->
    eval_expr sched ~in_container (f x)
  | Pair (x, y) ->
    let x = eval_expr sched ~in_container x
    and y = eval_expr sched ~in_container y in
    x >>= fun x ->
    y >>= fun y ->
    return (x, y)
  | Workflow_path w ->
    submit_for_eval sched w >>= fun trace ->
    if Execution_trace.is_errored trace then
      fail (Execution_trace.gather_failures [w] [trace])
    else
      return (Db.path sched.config.db w)
  | Spawn (xs, f) ->
    eval_expr sched ~in_container:false xs >>= fun xs ->
    let ys = List.map xs ~f in
    map_p ys ~f:(eval_expr ~in_container sched)
  | List xs ->
    map_p ~f:(eval_expr ~in_container sched) xs

  | Glob { dir ; _ } ->
    submit_for_eval sched dir >>= fun _ ->
    Misc.files_in_dir (workflow_path sched ~in_container:false dir) >> fun files ->
    return @@ List.map files ~f:(fun f -> f, Workflow.select dir [f])

and workflow_path
  : type s. t -> in_container:bool -> s Workflow.t -> string
  = fun sched ~in_container w ->
    if in_container then Execution_env.((container_mount sched.config.db w).file_container_location)
    else Db.path sched.config.db w

and task_trace sched t =
  let ready = Unix.gettimeofday () in
  log ~time:ready sched (Logger.Task_ready t) ;
  Allocator.request sched.allocator (Task.requirement t) >>= function
  | Ok resource ->
    let start = Unix.gettimeofday () in
    log ~time:start sched (Logger.Task_started (t, resource)) ;
    Task.perform t sched.config resource >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    log ~time:_end_ sched (Logger.Task_ended { outcome ; start ; _end_ }) ;
    Allocator.release sched.allocator resource ;
    Lwt.return (
      Execution_trace.Run { ready ; start  ; _end_ ; outcome }
    )
  | Error (`Msg msg) ->
    log sched (Logger.Task_allocation_error (t, msg)) ;
    Lwt.return (Execution_trace.Allocation_error msg)

let eval_expr sched e =
  let f ids =
    let ids = S.to_list ids in
    Lwt_list.map_p (fun id -> Table.find_exn sched.traces id) ids >|= fun traces ->
    List.zip_exn ids traces
  in
  Lwt_result.bind_lwt_err (eval_expr sched ~in_container:false e) f

let create
    ?(loggers = [])
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ?(use_docker = true) db =
  {
    allocator = Allocator.create ~np ~mem:(mem * 1024) ;
    config = { Task.db ; use_docker } ;
    start_signal = Semaphore.create false ;
    traces = String.Table.create () ;
    logger = Logger.tee loggers ;
  }

let join sched =
  Table.to_alist sched.traces
  |> List.map ~f:(fun (_, trace) -> trace >|= ignore)
  |> Lwt.join

let start sched =
  Semaphore.go sched.start_signal

let error_report db traces =
  let buf = Buffer.create 1024 in
  List.iter traces ~f:(fun (id, trace) ->
      Execution_trace.error_report trace db buf id
    ) ;
  Buffer.contents buf

let eval_expr_main ?np ?mem ?loggers ?use_docker db expr =
  let sched = create ?loggers ?np ?mem ?use_docker db in
  let t = eval_expr sched expr in
  start sched ;
  match Lwt_main.run t with
  | Ok _ as x -> x
  | Error traces -> Error (error_report db traces)



(* let wait4deps sched w =
 *   let check_outcome w =
 *     match Table.find sched.traces (Workflow.id w) with
 *     | Some trace -> (
 *         trace >|= function
 *         | Skipped `Done_already -> true
 *         | Skipped (`Missing_dep | `Allocation_error _) -> false
 *         | Run { outcome } -> Task_result.succeeded outcome
 *       )
 *     | None ->
 *       (\* never call [wait4deps] before having registered the deps *\)
 *       assert false
 *   in
 *   let deps = Command.deps w.Workflow.task in
 *   Lwt_list.map_p check_outcome deps >|= fun xs ->
 *   if List.for_all xs ~f:ident then `All_deps_cleared
 *   else `Some_dep_failed
 *
 * let rec register sched (Workflow.Any w) =
 *   let is_registered = Table.mem sched.traces (Workflow.id w) in
 *   if not is_registered then (
 *     let trace =
 *       Semaphore.wait sched.start_signal >>= fun () ->
 *       Task.is_done w sched.config.db >>= fun is_done ->
 *       if is_done then
 *         Lwt.return (Execution_trace.Skipped `Done_already)
 *       else
 *         let open Execution_trace in
 *         List.iter ~f:(register sched) (Workflow.deps w) ;
 *         wait4deps sched w >>= function
 *         | `All_deps_cleared -> Workflow.(
 *             match w with
 *             | Input _ | Select _ | Shell _ ->
 *               regular_trace sched (Workflow.Any w)
 *             | Plugin _ -> assert false
 *           )
 *         | `Some_dep_failed ->
 *           Lwt.return (Execution_trace.Skipped `Missing_dep)
 *     in
 *     Table.set sched.traces (Workflow.id w) trace
 *   )
 *
 * and regular_trace sched (Workflow.Any w) =
 *   let open Execution_trace in
 *   let ready = Unix.gettimeofday () in
 *   Allocator.request sched.allocator (Task.requirement w) >>= function
 *   | Ok resource ->
 *     let start = Unix.gettimeofday () in
 *     Workflow.(
 *       match w with
 *       | Input { path ; _ } -> Task.perform_input path
 *       | Select { dir ; path ; _ } ->
 *         Task.perform_select sched.config.db dir path
 *       | Shell shell ->
 *         Task.perform_shell sched.config resource shell
 *       | Plugin _ -> assert false
 *     ) >>= fun outcome ->
 *     let _end_ = Unix.gettimeofday () in
 *     Allocator.release sched.allocator resource ;
 *     Lwt.return (
 *       Execution_trace.Run { ready ; start  ; _end_ = 0. ; outcome }
 *     )
 *   | Error (`Msg msg) ->
 *           let err = `Allocation_error msg in
 *           Lwt.return (Execution_trace.Skipped err) *)

(* and map_dir_trace sched w ~dir ~f =
 *   let ready = Unix.gettimeofday () in
 *   let dir_path = Workflow.path sched.config.db dir in
 *   Misc.files_in_dir dir_path >>= fun files_in_dir ->
 *   let inputs = List.map files_in_dir ~f:(fun fn ->
 *       Workflow.input (Filename.concat dir_path fn)
 *     )
 *   in
 *   let goals = List.map inputs ~f in
 *   List.map ~f:(register sched) goals |> Lwt.join >>= fun () ->
 *   Lwt_list.map_p
 *     (fun w -> Table.find_exn sched.traces (Workflow.id w))
 *     goals >>= fun traces ->
 *   (
 *     match List.find traces ~f:Execution_trace.is_errored with
 *     | None ->
 *       let start = Unix.gettimeofday () in
 *       Task.perform_map_dir sched.config.db ~files_in_dir ~goals w >>= fun outcome ->
 *       let _end_ = Unix.gettimeofday () in
 *       (\* FIXME: delete all intermediates *\)
 *       Lwt.return (Execution_trace.Run { ready ; start ; _end_ ; outcome })
 *
 *     | Some failed_trace ->
 *       let _end_ = Unix.gettimeofday () in
 *       let tr = Execution_trace.(Run {
 *           ready ;
 *           start = _end_ ;
 *           _end_ ;
 *           outcome = `Map_dir { Task_result.Map_dir.pass = false ;
 *                                cache = None }
 *         })
 *       in
 *       Lwt.return tr
 *   ) *)
