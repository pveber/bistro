open Core_kernel
open Lwt.Infix
open Bistro_base

module S = Caml.Set.Make(struct
    type t = Execution_trace.t
    let compare = compare
  end)


module Table = String.Table

module Lwt_eval = struct
  type 'a t = ('a, S.t) Lwt_result.t (* missing deps *)
  let ( >> ) = Lwt.( >>= )
  let ( >>= ) = Lwt_result.( >>= )
  let ( >>| ) = Lwt_result.( >|= )

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

module Gc : sig
  type t
  val create : Db.t -> t
  val protect : t -> Workflow.t -> unit
  val uses : t -> Workflow.t -> Workflow.t -> unit
end
=
struct
  module Dep = struct
    include Workflow

    let equal x y =
      String.equal (id x) (id y)

    let hash w =
      Hashtbl.hash (Workflow.digestible_workflow w)
  end

  module T = Caml.Hashtbl.Make(Dep)
  module S = Caml.Set.Make(Dep)
  type t = {
    db : Db.t ;
    revdeps : S.t T.t ;
    mutable protected : S.t ;
  }
  let create db = {
    db ;
    revdeps = T.create 253 ;
    protected = S.empty ;
  }

  let protect gc x =
    gc.protected <- S.add x gc.protected

  let uses gc x y =
    match T.find gc.revdeps y with
    | exception Caml.Not_found -> T.add gc.revdeps y (S.singleton x)
    | s -> T.replace gc.revdeps y (S.add x s)

  (* let rec fold_dep_aux seen d ~init ~f =
   *   if S.mem d seen then (seen, init)
   *   else (
   *       List.fold (Workflow.deps d) ~init:(seen, init) ~f:(fun (seen, init) d -> fold_dep_aux seen d ~init ~f)
   *   )
   * 
   * let fold_dep d ~init ~f =
   *   let seen = S.empty in
   *   fold_dep_aux seen d ~init ~f
   *   |> snd *)
end

type t = {
  allocator : Allocator.t ;
  config : Task.config ;
  traces : Execution_trace.t Lwt.t Table.t ;
  logger : Logger.t ;
}

let log ?(time = Unix.gettimeofday ()) sched event =
  sched.logger#event sched.config.db time event

let create
    ?(loggers = [])
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ?(use_docker = true) db =
  {
    allocator = Allocator.create ~np ~mem:(mem * 1024) ;
    config = { Task.db ; use_docker } ;
    traces = String.Table.create () ;
    logger = Logger.tee loggers ;
  }

let error_report db traces =
  let buf = Buffer.create 1024 in
  List.iter traces ~f:(fun trace ->
      Execution_trace.error_report trace db buf
    ) ;
  Buffer.contents buf

let task_trace sched w t =
  let ready = Unix.gettimeofday () in
  log ~time:ready sched (Logger.Workflow_ready w) ;
  Allocator.request sched.allocator (Task.requirement t) >>= function
  | Ok resource ->
    let start = Unix.gettimeofday () in
    log ~time:start sched (Logger.Workflow_started (w, resource)) ;
    Task.perform t sched.config resource >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    log ~time:_end_ sched (Logger.Workflow_ended { outcome ; start ; _end_ }) ;
    Allocator.release sched.allocator resource ;
    Lwt.return (
      Execution_trace.Run { ready ; start  ; _end_ ; outcome }
    )
  | Error (`Msg msg) ->
    log sched (Logger.Workflow_allocation_error (w, msg)) ;
    Lwt.return (Execution_trace.Allocation_error (w, msg))

let rec schedule_workflow sched w =
  let id = Workflow.id w in
  (
    match Table.find sched.traces id with
    | None ->
      let trace = workflow_trace sched w in
      Table.set sched.traces ~key:id ~data:trace ;
      trace
    | Some trace -> trace
  ) >>= fun trace ->
  if Execution_trace.is_errored trace then
    Lwt_eval.fail1 trace
  else
    Lwt_result.return ()

and workflow_trace sched w =
  Task.is_done w sched.config.db >>= fun is_done ->
  if is_done then (
    log sched (Logger.Workflow_skipped (w, `Done_already)) ;
    Lwt.return (Execution_trace.Done_already w)
  )
  else (
    Lwt_eval.(
      schedule_deps sched w >>= fun _ ->
      task_of_workflow sched w
    ) >>=function
    | Ok t -> task_trace sched w t
    | Error missing_deps ->
      log sched Logger.(Workflow_skipped (w, `Missing_dep)) ;
      Lwt.return (Execution_trace.Canceled { workflow = w ; missing_deps = S.elements missing_deps})
  )

and task_of_workflow sched =
  let open Lwt_eval in
  function
  | Workflow.Input { id ; path } -> return (Task.input ~id ~path)
  | Select { dir ; sel ; id } -> return (Task.select ~id ~dir ~sel)
  | Shell { id ; descr ; np ; mem ; task = cmd ; _ } ->
    return (Task.shell ~id ~descr ~np ~mem cmd)
  | Plugin { id ; descr ; np ; mem ; task = f ; _ } ->
    return (Task.plugin ~id ~descr ~np ~mem f)
  | MapDir md ->
    Misc.files_in_dir (Db.path sched.config.db md.dir) >> fun files ->
    let names = match md.ext with
      | None -> files
      | Some ext ->
        let f fn = (Filename.remove_extension fn) ^ "." ^ ext in
        List.map ~f files
    in
    let inputs = List.map files ~f:Bistro.(fun f ->
        select (Private.laever md.dir) (selector [f])
      ) in
    let targets = List.map ~f:(fun x -> md.f (Bistro.Private.reveal x)) inputs in
    Lwt_eval.map_p targets ~f:(fun w -> schedule_workflow sched w) >>= fun _ ->
    return (Task.mapdir ~id:md.id ~names ~targets)

and schedule_deps sched w =
  let deps = Workflow.deps w in
  Lwt_eval.map_p ~f:(schedule_workflow sched) deps >>= function
  | Ok _ -> Lwt_eval.return ()
  | Error traces -> Lwt_eval.fail (S.filter Execution_trace.is_errored traces)

module Expr = struct
  type scheduler = t
  type _ t =
    | Pure : 'a -> 'a t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Workflow : _ Bistro.workflow -> string t
    | List : 'a t list -> 'a list t

  let rec eval
    : type s. scheduler -> s t -> (s, S.t) Lwt_result.t
    = fun sched expr ->
      let open Lwt_eval in
      match expr with
      | Pure x -> return x
      | App (f, x) ->
        let f = eval sched f and x = eval sched x in
        f >>= fun f ->
        x >>= fun x -> (* FIXME: ici il faudrait renvoyer l'union des erreurs *)
        return (f x)
      | Workflow w ->
        let w = Bistro.Private.reveal w in
        schedule_workflow sched w >>= fun _ ->
        return (Db.path sched.config.db w)
      | List xs ->
        map_p xs ~f:(eval sched)

  let pure x = Pure x
  let app f x = App (f, x)
  let ( $ ) = app
  let map x ~f = pure f $ x
  let both x y = pure (fun x y -> x, y) $ x $ y
  let dep x = Workflow x
  let list xs = List xs
  let return x = pure x
end

module Let_syntax = struct
  module Let_syntax = struct
    include Expr
    module Open_on_rhs = struct
      include Expr
    end
  end
end

let eval ?loggers ?np ?mem ?use_docker db expr =
  let sched = create ?loggers ?np ?mem ?use_docker db in
  Expr.eval sched expr

let eval_main ?np ?mem ?loggers ?use_docker db expr =
  let sched = create ?loggers ?np ?mem ?use_docker db in
  let t = Expr.eval sched expr in
  match Lwt_main.run t with
  | Ok _ as x -> x
  | Error traces -> Error (error_report db (S.elements traces))

(* let rec submit sched w : Execution_trace.t Lwt.t =
 *   match Table.find sched.traces (Workflow.id w) with
 *   | None ->
 *     let trace =
 *       Semaphore.wait sched.start_signal >>= fun () ->
 *       Task.is_done w sched.config.db >>= fun is_done ->
 *       if is_done then (
 *         log sched (Logger.Workflow_skipped (w, `Done_already)) ;
 *         Lwt.return Execution_trace.Done_already
 *       )
 *       else (
 *         compute_deps sched w >>= function
 *         | Ok _ ->
 *           task_trace sched (compute_task w)
 *         | Error missing_deps ->
 *           log sched Logger.(Workflow_skipped (w, `Missing_dep)) ;
 *           Lwt.return (Execution_trace.Canceled { missing_deps })
 *       )
 *     in
 *     Table.set sched.traces ~key:(Workflow.id w) ~data:trace ;
 *     trace
 *   | Some trace -> trace
 *
 * and compute_deps sched w =
 *   let deps = Workflow.deps w in
 *   Lwt_eval.map_p ~f:(submit_for_eval sched) deps
 *
 * and compute_task = function
 *   | Input { path ; id } ->
 *     Task.input ~path ~id
 *   | Select { dir ; sel ; _ } ->
 *     Task.select ~dir ~sel
 *   | Shell { task = cmd ; id ; descr ; np ; mem ; _ } ->
 *     Task.shell ~id ~descr ~np ~mem cmd
 *   | Plugin { task = f ; id ; descr ; np ; mem; _} ->
 *     Task.plugin ~id ~descr ~np ~mem f
 *
 * and submit_for_eval sched w =
 *   let open Lwt_eval in
 *   submit sched w >> fun trace ->
 *   if Execution_trace.is_errored trace then
 *     fail (Execution_trace.gather_failures [w] [trace])
 *   else
 *     return trace
 *
 *
 * let rec eval_expr : type s. t -> s Expr.t -> s Lwt_eval.t = fun sched expr ->
 *   let open Expr in
 *   let open Lwt_eval in
 *   match expr with
 *   | Pure x -> return x
 *   | App (f, x) ->
 *     let f = eval_expr sched f in
 *     let x = eval_expr sched x in
 *     x >>= fun x ->
 *     f >>= fun f ->
 *     return (f x)
 *   | Pair (x, y) ->
 *     let x = eval_expr sched x
 *     and y = eval_expr sched y in
 *     x >>= fun x ->
 *     y >>= fun y ->
 *     return (x, y)
 *   | Workflow_path w ->
 *     eval_expr sched w >>= fun w ->
 *     let w = Bistro.Private.reveal w in
 *     submit_for_eval sched w >>= fun trace ->
 *     if Execution_trace.is_errored trace then
 *       fail (Execution_trace.gather_failures [w] [trace])
 *     else
 *       return (Db.path sched.config.db w)
 *   | Spawn (xs, f) ->
 *     eval_expr sched xs >>= fun xs ->
 *     let ys = Base.List.map xs ~f:(fun x -> f (pure x)) in
 *     map_p ys ~f:(eval_expr sched)
 *   | List xs ->
 *     map_p ~f:(eval_expr sched) xs
 *
 *   | Glob { dir ; _ } ->
 *     eval_expr sched dir >>= fun dir ->
 *     submit_for_eval sched (Bistro.Private.reveal dir) >>= fun _ ->
 *     Misc.files_in_dir (workflow_path sched dir) >> fun files ->
 *     return @@ Base.List.map files ~f:(fun f -> f, Bistro.(dir /> selector [f]))
 *
 * and workflow_path
 *   : type s. t -> s Bistro.workflow -> string
 *   = fun sched w ->
 *     let w = Bistro.Private.reveal w in
 *     (* if in_container then Execution_env.((container_mount sched.config.db w).file_container_location)
 *        else *) Db.path sched.config.db w
 *
 * let submit sched w = submit sched (Bistro.Private.reveal w)
 *
 * let eval_expr sched e =
 *   let f ids =
 *     let ids = S.to_list ids in
 *     Lwt_list.map_p (fun id -> Table.find_exn sched.traces id) ids >|= fun traces ->
 *     List.zip_exn ids traces
 *   in
 *   Lwt_result.bind_lwt_err (eval_expr sched e) f *)




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
 *       (* never call [wait4deps] before having registered the deps *)
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
 *       (* FIXME: delete all intermediates *)
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
