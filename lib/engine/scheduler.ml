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
  mutable goals : Workflow.any list ;
  traces : Execution_trace.t Lwt.t Table.t ;
  start_signal : Semaphore.t ;
}

let null_logger _ _ = ()


let wait4deps sched w =
  let check_outcome (Workflow.Any w) =
    match Table.find sched.traces (Workflow.id w) with
    | Some trace -> (
        trace >|= function
        | Skipped `Done_already -> true
        | Skipped (`Missing_dep | `Allocation_error _) -> false
        | Run { outcome } -> Task_result.succeeded outcome
      )
    | None ->
      (* never call [wait4deps] before having registered the deps *)
      assert false
  in
  let deps = Workflow.deps w in
  Lwt_list.map_p check_outcome deps >|= fun xs ->
  if List.for_all xs ~f:ident then `All_deps_cleared
  else `Some_dep_failed

let rec register sched (Workflow.Any w) =
  let is_registered = Table.mem sched.traces (Workflow.id w) in
  if not is_registered then (
    let trace =
      Semaphore.wait sched.start_signal >>= fun () ->
      Task.is_done w sched.config.db >>= fun is_done ->
      if is_done then
        Lwt.return (Execution_trace.Skipped `Done_already)
      else
        let open Execution_trace in
        List.iter ~f:(register sched) (Workflow.deps w) ;
        wait4deps sched w >>= function
        | `All_deps_cleared -> Workflow.(
            match w with
            | Input _ | Select _ | Shell _ ->
              regular_trace sched (Workflow.Any w)
            | Closure _ -> assert false
          )
        | `Some_dep_failed ->
          Lwt.return (Execution_trace.Skipped `Missing_dep)
    in
    Table.set sched.traces (Workflow.id w) trace
  )

and regular_trace sched (Workflow.Any w) =
  let open Execution_trace in
  let ready = Unix.gettimeofday () in
  Allocator.request sched.allocator (Task.requirement w) >>= function
  | Ok resource ->
    let start = Unix.gettimeofday () in
    Workflow.(
      match w with
      | Input { path ; _ } -> Task.perform_input path
      | Select { dir ; path ; _ } ->
        Task.perform_select sched.config.db dir path
      | Shell shell ->
        Task.perform_shell sched.config resource shell
      | Closure _ -> assert false
    ) >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    Allocator.release sched.allocator resource ;
    Lwt.return (
      Execution_trace.Run { ready ; start  ; _end_ = 0. ; outcome }
    )
  | Error (`Msg msg) ->
          let err = `Allocation_error msg in
          Lwt.return (Execution_trace.Skipped err)

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

let create
    ?(loggers = [])
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ~db ~use_docker () =
  {
    allocator = Allocator.create ~np ~mem:(mem * 1024) ;
    config = { Task.db ; use_docker } ;
    goals = [] ;
    start_signal = Semaphore.create false ;
    traces = String.Table.create () ;
  }

let submit sched w =
  let w = Workflow.Any w in
  sched.goals <- w :: sched.goals ;
  register sched w

let find_trace sched (Workflow.Any w) =
  String.Table.find_exn sched.traces (Workflow.id w)

let join sched =
  Table.to_alist sched.traces
  |> List.map ~f:(fun (wid, trace) -> trace >|= fun t -> wid, t)
  |> Lwt_list.map_p ident

let start sched =
  Table.to_alist sched.traces
  |> List.map ~f:(fun (_, trace) -> trace >|= ignore)
  |> Lwt.join

