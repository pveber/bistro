open Core_kernel
open Lwt
open Bistro_base

type time = float

type event =
  | Start
  | End

type logger = time -> event -> unit

module Event = struct
  type 'a t = {
    value : 'a Lwt.t ;
    send : 'a Lwt.u ;
  }

  let create () =
    let value, send = Lwt.wait () in
    { value ; send }

end

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

  let stop s =
    s.state <- false

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
}

let null_logger _ _ = ()


module Lwt_eval = struct
  type 'a t = ('a, unit) Pervasives.result Lwt.t

  let ( >>= ) = Lwt.( >>= )
  let ( >>=? ) = Lwt_result.( >>= )
  let ( >|=? ) = Lwt_result.( >|= )

  let return_ok = Lwt_result.return
  let return_failure = Lwt_result.fail

  let map_p xs ~f =
    Lwt.bind (Lwt_list.map_p f xs) @@ fun results ->
    let res =
      try Ok (List.map results ~f:(function Ok x -> x | Error _ -> failwith ""))
      with Failure _ -> Error ()
    in
    Lwt.return res
end

let rec submit sched w =
  match Table.find sched.traces (Workflow.id w) with
  | None ->
    let trace =
      Semaphore.wait sched.start_signal >>= fun () ->
      Task.is_done w sched.config.db >>= fun is_done ->
      if is_done then
        Lwt.return (Execution_trace.Skipped `Done_already)
      else (
        let open Execution_trace in
        compute_task sched w >>= function
        | Ok t ->
          task_trace sched t
        | Error () ->
          Lwt.return (Execution_trace.Skipped `Missing_dep)
      )
    in
    Table.set sched.traces (Workflow.id w) trace ;
    trace
  | Some trace -> trace


and compute_task sched =
  let open Lwt_eval in function
  | Input { path ; id } -> Lwt_result.return @@ Task.input ~path ~id
  | Select { dir ; path } ->
    on_dep_outcome sched [ submit sched dir ] @@ fun () ->
    Task.select ~dir ~path
  | Shell { task = cmd ; } ->
    eval_command sched cmd >>=? fun cmd ->
    Lwt_result.return @@ Task.shell cmd
  | Closure _ -> assert false

and on_dep_outcome sched deps f = (* FIXME: pas vraiment nécessaire *)
  let check_outcome trace =
    trace >|= function
    | Execution_trace.Skipped `Done_already -> true
    | Skipped (`Missing_dep | `Allocation_error _) -> false
    | Run { outcome } -> Task_result.succeeded outcome
  in
  Lwt_list.map_p check_outcome deps >>= fun xs ->
  if List.for_all xs ~f:ident then (Lwt_result.return (f()))
  else Lwt_result.fail ()

and eval_command sched =
  let open Command in
  let open Lwt_eval in
  function
  | Simple_command tmpl ->
    eval_template sched tmpl >|=? fun tmpl ->
    Simple_command tmpl
  | Docker (img, c) ->
    eval_command sched c >|=? fun c ->
    Docker (img, c)
  | And_list cs ->
    map_p cs ~f:(eval_command sched) >|=? fun cs ->
    And_list cs
  | Pipe_list cs ->
    map_p cs ~f:(eval_command sched) >|=? fun cs ->
    Pipe_list cs
  | Or_list cs ->
    map_p cs ~f:(eval_command sched) >|=? fun cs ->
    Or_list cs

and eval_template sched xs =
  Lwt_eval.map_p xs ~f:(eval_token sched)

and eval_token sched =
  let open Template in
  let open Lwt_eval in
  function
  | D expr -> eval_expr sched expr >|=? fun id -> D id
  | F tmpl -> eval_template sched tmpl >|=? fun tmpl -> F tmpl
  | (S _ | DEST | TMP | NP | MEM as x) -> return_ok x

and eval_expr : type s. t -> s Workflow.expr -> (s, unit) Lwt_result.t = fun sched expr ->
  let open Workflow in
  let open Lwt_eval in
  match expr with
  | Pure { value } -> Lwt_result.return value
  | App (f, x) ->
    eval_expr sched f >>=? fun f ->
    eval_expr sched x >>=? fun x ->
    return_ok (f x)
  | List xs ->
    map_p ~f:(eval_expr sched) xs

  | Glob { dir ; pattern } ->
    let open Lwt in
    submit sched dir >>= fun _ ->
    assert false

  | Map_workflows { xs ; f } ->
    eval_expr sched xs >>=? fun sources ->
    let targets = List.map ~f sources in
    Lwt_list.map_p (submit sched) targets >>= fun traces ->
    if Execution_trace.all_ok traces then
      return_ok targets
    else
      return_failure ()
  | Dep w ->
    eval_expr sched w >>=? fun w ->
    submit sched w >>= fun trace ->
    if Execution_trace.is_errored trace then
      return_failure ()
    else
      return_ok (Db.path sched.config.db w)
  | Deps ws ->
    eval_expr sched ws >>=? fun ws ->
    Lwt_list.map_p (submit sched) ws >>= fun traces ->
    if Execution_trace.all_ok traces then
      return_ok (List.map ws ~f:(Db.path sched.config.db))
    else
      return_failure ()

and task_trace sched t =
  let open Execution_trace in
  let ready = Unix.gettimeofday () in
  Allocator.request sched.allocator (Task.requirement t) >>= function
  | Ok resource ->
    let start = Unix.gettimeofday () in
    Task.perform t >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    Allocator.release sched.allocator resource ;
    Lwt.return (
      Execution_trace.Run { ready ; start  ; _end_ = 0. ; outcome }
    )
  | Error (`Msg msg) ->
    let err = `Allocation_error msg in
    Lwt.return (Execution_trace.Skipped err)

let create
    ?(loggers = [])
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ?(use_docker = true) db =
  {
    allocator = Allocator.create ~np ~mem:(mem * 1024) ;
    config = { Task.db ; use_docker } ;
    start_signal = Semaphore.create false ;
    traces = String.Table.create () ;
  }

let join sched =
  Table.to_alist sched.traces
  |> List.map ~f:(fun (wid, trace) -> trace >|= ignore)
  |> Lwt.join

let start sched =
  Semaphore.go sched.start_signal













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
