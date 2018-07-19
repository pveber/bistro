open Core_kernel
open Lwt
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

type t = {
  allocator : Allocator.t ;
  config : Task.config ;
  traces : Execution_trace.t Lwt.t Table.t ;
  start_signal : Semaphore.t ;
  logger : Logger.t ;
}

module Lwt_eval = struct
  type 'a t = ('a, S.t) Lwt_result.t (* missing deps *)
  let ( >> ) = Lwt.( >>= )
  let ( >>= ) = Lwt_result.( >>= )
  let ( >|= ) = Lwt_result.( >|= )

  let return = Lwt_result.return
  let fail = Lwt_result.fail
  let fail1 e = Lwt_result.fail (S.singleton e)

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
        compute_task sched w >>= function
        | Ok t ->
          task_trace sched t
        | Error missing_deps ->
          log sched Logger.(Workflow_skipped (w, `Missing_dep)) ;
          Lwt.return (Execution_trace.Canceled { missing_deps })
      )
    in
    Table.set sched.traces ~key:(Workflow.id w) ~data:trace ;
    trace
  | Some trace -> trace


and compute_task sched =
  let open Lwt_eval in function
  | Input { path ; id } -> return @@ Task.input ~path ~id
  | Select { dir ; sel ; _ } ->
    submit_for_eval sched dir >>= fun _ ->
    return @@ Task.select ~dir ~sel
  | Shell { task = cmd ; id ; descr ; np ; mem ; _ } ->
    eval_command sched ~in_container:false cmd >>= fun cmd ->
    return @@ Task.shell ~id ~descr ~np ~mem cmd
  | Closure { task = f ; id ; descr ; np ; mem; _} ->
    eval_expr sched ~in_container:false f >>= fun f ->
    return @@ Task.closure ~id ~descr ~np ~mem f

and submit_for_eval sched w =
  let open Lwt_eval in
  submit sched w >> fun trace ->
  if Execution_trace.is_errored trace then
    fail (Execution_trace.gather_failures [w] [trace])
  else
    return trace

and eval_command : type s. t -> in_container:bool -> s Workflow.expr Command.t -> s Command.t Lwt_eval.t = fun sched ~in_container ->
  let open Command in
  let open Lwt_eval in
  function
  | Simple_command tmpl ->
    eval_template sched ~in_container tmpl >|= fun tmpl ->
    Simple_command tmpl
  | Docker (img, c) ->
    eval_command ~in_container:sched.config.use_docker sched c >|= fun c ->
    Docker (img, c)
  | And_list cs ->
    map_p cs ~f:(eval_command ~in_container sched) >|= fun cs ->
    And_list cs
  | Pipe_list cs ->
    map_p cs ~f:(eval_command ~in_container sched) >|= fun cs ->
    Pipe_list cs
  | Or_list cs ->
    map_p cs ~f:(eval_command ~in_container sched) >|= fun cs ->
    Or_list cs

and eval_template : type s. t -> in_container:bool -> s Workflow.expr Template.t -> s Template.t Lwt_eval.t = fun sched ~in_container xs ->
  Lwt_eval.map_p xs ~f:(eval_token sched ~in_container)

and eval_token : type s. t -> in_container:bool -> s Workflow.expr Template.token -> s Template.token Lwt_eval.t = fun sched ~in_container ->
  let open Template in
  let open Lwt_eval in
  function
  | D expr -> eval_expr sched ~in_container expr >|= fun path -> D path
  | F tmpl -> eval_template sched ~in_container tmpl >|= fun tmpl -> F tmpl
  | (S _ | DEST | TMP | NP | MEM as x) -> return x

and eval_expr : type s. t -> in_container:bool -> s Workflow.expr -> s Lwt_eval.t = fun sched ~in_container expr ->
  let open Workflow in
  let open Lwt_eval in
  match expr with
  | Pure { value ; _ } -> Lwt_result.return value
  | App (f, x) ->
    let f = eval_expr sched ~in_container f
    and x = eval_expr sched ~in_container x in
    f >>= fun f ->
    x >>= fun x ->
    return_ok (f x)
  | List xs ->
    map_p ~f:(eval_expr ~in_container sched) xs

  | Glob { dir ; _ } ->
    submit_for_eval sched dir >>= fun _ ->
    Misc.files_in_dir (workflow_path sched ~in_container:false dir) >> fun files ->
    return @@ List.map files ~f:(fun f -> Workflow.select dir [f])

  | Map_workflows { xs ; f } ->
    eval_expr sched ~in_container xs >>= fun sources ->
    let targets = List.map ~f sources in
    map_p ~f:(submit_for_eval sched) targets >>= fun traces ->
    if Execution_trace.all_ok traces then
      return targets
    else
      Lwt_eval.fail (Execution_trace.gather_failures targets traces)

  | Map2_workflows { xs ; ys ; f } ->
    let xs = eval_expr sched ~in_container xs in
    let ys = eval_expr sched ~in_container ys in
    xs >>= fun xs ->
    ys >>= fun ys ->
    let targets = List.map2_exn ~f xs ys in
    map_p ~f:(submit_for_eval sched) targets >>= fun traces ->
    if Execution_trace.all_ok traces then
      return targets
    else
      Lwt_eval.fail (Execution_trace.gather_failures targets traces)
  | Path_of_dep -> return (Db.path sched.config.db)
  | Map_list { xs ; f } ->
    let xs = eval_expr sched ~in_container xs in
    let f  = eval_expr sched ~in_container f  in
    xs >>= fun xs ->
    f  >>= fun f  ->
    return (List.map xs ~f)
    
  (* | Dep w -> *)
  (*   eval_expr sched ~in_container:false w >>= fun w -> *)
  (*   submit_for_eval sched w >>= fun trace -> *)
  (*   if Execution_trace.is_errored trace then *)
  (*     fail (Execution_trace.gather_failures [w] [trace]) *)
  (*   else *)
  (*     return (dep_path sched ~in_container w) *)
  (* | Deps ws -> *)
  (*   eval_expr sched ~in_container:false ws >>= fun ws -> *)
  (*   map_p ~f:(submit_for_eval sched) ws >>= fun traces -> *)
  (*   if Execution_trace.all_ok traces then *)
  (*     return (List.map ws ~f:(dep_path sched ~in_container)) *)
  (*   else *)
  (*     fail (Execution_trace.gather_failures ws traces) *)

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
 *             | Closure _ -> assert false
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
 *       | Closure _ -> assert false
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
