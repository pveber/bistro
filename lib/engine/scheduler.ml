open Core
open Lwt.Infix
open Bistro_internals
module W = Bistro_internals.Workflow

type error = [
  | `Msg of string
]

module Table = String.Table

module Traces = Caml.Set.Make(struct
    type t = Execution_trace.t
    let compare = compare
  end)

(* Lwt threads that accumulate errors *)
module Eval_thread : sig
  type 'a t = ('a, Traces.t) Lwt_result.t
  val return : 'a -> 'a t
  (* val fail : Traces.t -> 'a t *)
  val fail1 : Execution_trace.t -> 'a t
  val both : 'a t -> 'b t -> ('a * 'b) t
  (* val list_map : *)
  (*   'a list -> *)
  (*   f:('a -> 'b t) -> *)
  (*   'b list t *)
  val join :
    'a list ->
    f:('a -> unit t) ->
    unit t
  val ignore : 'a t -> unit t
  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >> ) : 'a Lwt.t -> ('a -> 'b t) -> 'b t
  end
end
=
struct
  type 'a t = ('a, Traces.t) Lwt_result.t

  module Infix = struct
    let ( >> ) = Lwt.( >>= )
    let ( >>= ) = Lwt_result.( >>= )
    let ( >>| ) = Lwt_result.( >|= )
  end

  let return = Lwt_result.return
  (* let fail = Lwt_result.fail *)
  let fail1 e = Lwt_result.fail (Traces.singleton e)
  let result_both x y =
    match x, y with
    | Ok x, Ok y -> Ok (x, y)
    | Ok _, Error e -> Error e
    | Error e, Ok _ -> Error e
    | Error e, Error e' -> Error (Traces.union e e')

  let both x y =
    Lwt.(x >>= fun x ->
         y >>= fun y ->
         return (result_both x y))

  let list_map xs ~f =
    Lwt.bind (Lwt_list.map_p f xs) @@ fun results ->
    let res =
      List.fold results ~init:(Ok []) ~f:(fun acc res ->
          Result.map (result_both acc res) ~f:(fun (xs, x) -> x :: xs)
        )
      |> (
        function
        | Ok xs -> Ok (List.rev xs)
        | Error _ as e -> e
      )
    in
    Lwt.return res

  let join xs ~f =
    let open Lwt_result in
    list_map xs ~f >|= ignore

  let ignore x = Infix.(x >>| ignore)
end

type 'a thread = 'a Eval_thread.t

let lwt_both x y =
  x >>= fun x ->
  y >>= fun y ->
  Lwt.return (x, y)

module Lwt_queue : sig
  type 'a t
  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a Lwt.t
end
=
struct
  type 'a t = {
    queue : 'a Queue.t ;
    condition : unit Lwt_condition.t ;
  }

  let create () = {
    queue = Queue.create () ;
    condition = Lwt_condition.create () ;
  }

  let push q x =
    Queue.enqueue q.queue x ;
    Lwt_condition.signal q.condition ()

  let rec pop q =
    match Queue.dequeue q.queue with
    | None ->
      Lwt_condition.wait q.condition >>= fun () ->
      pop q
    | Some x -> Lwt.return x
end

module Gc : sig
  type t
  val create : Db.t -> (Logger.event -> unit) -> t
  val register : t -> ?target:_ W.t -> _ W.t -> unit Lwt.t
  val tag_as_built : t -> _ W.t -> unit
  val stop : t -> unit Lwt.t
  (* val fold_deps :
   *   t ->
   *   init:'a ->
   *   f:('a -> Workflow.any -> Workflow.any -> 'a) ->
   *   'a *)

  type state = {
    deps : (Workflow.any * Workflow.any) list ;
    protected : Workflow.any list ;
  }
  val state : t -> state
end
=
struct
  module S = W.Set
  module T = struct
    include W.Table
    let update t ~key ~default ~f =
      let data = match find t key with
        | d -> d
        | exception Caml.Not_found -> default
      in
      replace t key (f data)

    let adj_add t u v =
      update t ~key:u ~default:S.empty ~f:(S.add v)

    let adj_find t u = match find t u with
      | x -> x
      | exception Caml.Not_found -> S.empty

    let incr_count t u =
      update t ~key:u ~default:0 ~f:succ

    let decr_count t u =
      let n = match find t u with
        | n -> n - 1
        | exception Caml.Not_found -> assert false
      in
      replace t u n ;
      n
  end


  type msg =
    | Built : _ W.t -> msg
    | Stop : msg

  type t = {
    db : Db.t ;
    log : Logger.event -> unit ;
    depends_on : S.t T.t ;
    is_used_by : S.t T.t ;
    counts : int T.t ;
    mutable protected : S.t ;
    inbox : msg Lwt_queue.t ;
    end_listener : unit Lwt.u ;
    _end_ : unit Lwt.t ;
  }

  let stop x =
    Lwt_queue.push x.inbox Stop ;
    x._end_

  let update_counts_and_collect gc (Workflow.Any w as dep_w) =
    let n = T.decr_count gc.counts dep_w in
    if n = 0 then (
      gc.log (Logger.Workflow_collected w) ;
      if not (S.mem dep_w gc.protected) then
        Misc.remove_if_exists (Db.cache gc.db (W.id w))
      else
        Lwt.return ()
    )
    else Lwt.return ()

  let rec main gc =
    Lwt_queue.pop gc.inbox >>= function
    | Built w ->
      T.adj_find gc.depends_on (Workflow.Any w)
      |> S.elements
      |> List.map ~f:(update_counts_and_collect gc)
      |> Lwt.join >>= fun () ->
      main gc
    | Stop ->
      Lwt.wakeup gc.end_listener () ;
      Lwt.return ()

  let create db log =
    let inbox = Lwt_queue.create () in
    let counts = T.create 253 in
    let _end_, end_listener = Lwt.wait () in
    let gc = {
      db ;
      log ;
      depends_on = T.create 253 ;
      is_used_by = T.create 253 ;
      counts ;
      protected = S.empty ;
      inbox ;
      _end_ ;
      end_listener ;
    }
    in
    Lwt.async (fun () -> main gc) ;
    gc

  let tag_as_built gc w =
    Lwt_queue.push gc.inbox (Built w)

  let uses gc u v =
    match u with
    | None ->
      gc.protected <- S.add (Workflow.Any v) gc.protected
    | Some u ->
      let u = Workflow.Any u and v = Workflow.Any v in
      T.adj_add gc.depends_on u v ;
      T.adj_add gc.is_used_by v u ;
      T.incr_count gc.counts v

  let rec register : type u v. t -> ?target:u W.t -> v W.t -> unit Lwt.t = fun gc ?target w ->
    match w with
    | Pure _ -> Lwt.return ()
    | App app ->
      lwt_both (register gc ?target app.f) (register gc ?target app.x)
      >|= ignore
    | Both both ->
      lwt_both (register gc ?target both.fst) (register gc ?target both.snd)
      >|= ignore
    | List l ->
      List.map ~f:(register ?target gc) l.elts
      |> Lwt.join
    | Eval_path x -> register gc ?target x.workflow
    | Spawn x ->
      Lwt_list.iter_p (register_any gc ?target) x.deps
    | List_nth l ->
      register gc ?target l.elts
    | Input _ -> Lwt.return ()
    | Select x -> register gc ?target x.dir
    | Value v ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else register gc ~target:w v.task
    | Path p ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else register gc ~target:w p.task
    | Shell s ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else Lwt_list.iter_p (register_any gc ~target:w) s.deps
    | Glob g ->
      register gc ?target g.dir

  and register_any : type u. t -> ?target:u W.t -> W.any -> unit Lwt.t = fun gc ?target (Workflow.Any w) ->
    register gc ?target w

  and stop_register : type u. t -> u W.t -> bool = fun gc w ->
    let u = Workflow.Any w in
    T.mem gc.depends_on u || Db.is_in_cache gc.db u

  let register gc ?target w =
    register gc ?target w

  (* let fold_deps gc ~init ~f =
   *   T.fold
   *     (fun u deps acc -> S.fold (fun v acc -> f acc u v) deps acc)
   *     gc.depends_on
   *     init *)

  type state = {
    deps : (Workflow.any * Workflow.any) list ;
    protected : Workflow.any list ;
  }

  let state gc = {
    deps =
      T.to_seq gc.depends_on
      |> Seq.flat_map (fun (u, s) -> Seq.map (fun v -> u, v) (S.to_seq s))
      |> Caml.List.of_seq ;
    protected =
      S.elements gc.protected ;
  }

end

module Maybe_gc : sig
  type t = Gc.t option
  val register : t -> ?target:_ W.t -> _ W.t -> unit Lwt.t
  val tag_as_built : t -> _ W.t -> unit
  val stop : t -> unit Lwt.t
end
=
struct
  type t = Gc.t option
  let register o ?target w = match o with
    | Some gc -> Gc.register gc ?target w
    | None -> Lwt.return ()

  let tag_as_built o w = match o with
    | Some gc -> Gc.tag_as_built gc w
    | None -> ()

  let stop = function
    | Some gc -> Gc.stop gc
    | None -> Lwt.return ()
end


type 'a t = {
  target : 'a W.t ;
  allocator : Allocator.t ;
  db : Db.t ;
  logger : Logger.t ;
  use_docker : bool ;
  traces : Execution_trace.t thread Table.t ;
  gc : Gc.t option ;
}

let create
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ?(use_docker = true)
    ?(loggers = [])
    ?(collect = false) db target =
  let logger = Logger.tee loggers in
  {
    target = Bistro.Private.reveal target ;
    allocator = Allocator.create ~np ~mem:(mem * 1024) ;
    db ;
    use_docker ;
    traces = String.Table.create () ;
    logger ;
    gc =
      if collect then
        let gc_log event = logger#event db (Unix.gettimeofday ()) event in
        Some (Gc.create db gc_log)
      else None ;
  }

let gc_state sched = Option.map ~f:Gc.state sched.gc

let log ?(time = Unix.gettimeofday ()) sched event =
  sched.logger#event sched.db time event

let worker f x =
  let (read_from_child, write_to_parent) = Unix.pipe () in
  let (read_from_parent, write_to_child) = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
    Unix.close read_from_child ;
    Unix.close write_to_child ;
    let res =
      try f x ; Ok ()
      with e ->
        let msg =
          sprintf "%s\n%s"
            (Exn.to_string e)
            (Printexc.get_backtrace ())
        in
        Error msg
    in
    let oc = Unix.out_channel_of_descr write_to_parent in
    Marshal.to_channel oc res [] ;
    Caml.flush oc ;
    Unix.close write_to_parent ;
    ignore (Caml.input_value (Unix.in_channel_of_descr read_from_parent)) ;
    assert false
  | `In_the_parent pid ->
    Unix.close write_to_parent ;
    Unix.close read_from_parent ;
    let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input read_from_child in
    Lwt_io.read_value ic >>= fun (res : (unit, string) result) ->
    Caml.Unix.kill (Pid.to_int pid) Caml.Sys.sigkill;
    Misc.waitpid (Pid.to_int pid) >>= fun _ ->
    Unix.close read_from_child ;
    Unix.close write_to_child ;
    Lwt.return res

let load_value fn =
  In_channel.with_file fn ~f:Marshal.from_channel

let save_value ~data fn =
  Out_channel.with_file fn ~f:(fun oc -> Marshal.to_channel oc data [])

let step_outcome ~exit_code ~dest_exists=
  match exit_code, dest_exists with
    0, true -> `Succeeded
  | 0, false -> `Missing_output
  | _ -> `Failed

let perform_input sched ~path ~id =
  let pass = Sys.file_exists path = `Yes in
  (
    if pass then Misc.cp path (Db.cache sched.db id)
    else Lwt.return ()
  ) >>= fun () ->
  Eval_thread.return (
    Task_result.Input { id ; pass ; path }
  )

let perform_select sched ~id ~dir ~sel =
  let p = Filename.concat (Db.path sched.db dir) (Path.to_string sel) in
  let pass = Sys.file_exists p = `Yes in
  Eval_thread.return (
    Task_result.Select {
      id ;
      pass ;
      dir_path = Db.path sched.db dir ;
      sel ;
    }
  )

let perform_shell sched (Allocator.Resource { np ; mem }) ~id ~descr cmd =
  let env =
    Execution_env.make
      ~use_docker:sched.use_docker
      ~db:sched.db
      ~np ~mem ~id
  in
  let cmd = Shell_command.make env cmd in
  Shell_command.run cmd >>= fun (exit_code, dest_exists) ->
  let cache_dest = Db.cache sched.db id in
  let outcome = step_outcome ~exit_code ~dest_exists in
  Misc.(
    if outcome = `Succeeded then
      mv env.dest cache_dest >>= fun () ->
      remove_if_exists env.tmp_dir
    else
      Lwt.return ()
  ) >>= fun () ->
  Eval_thread.return (Task_result.Shell {
      outcome ;
      id ;
      descr ;
      exit_code ;
      cmd = Shell_command.text cmd ;
      file_dumps = Shell_command.file_dumps cmd ;
      cache = if outcome = `Succeeded then Some cache_dest else None ;
      stdout = env.stdout ;
      stderr = env.stderr ;
    })

let rec blocking_evaluator
  : type s. Db.t -> s W.t -> (unit -> s)
  = fun db w ->
    match w with
    | W.Pure { value ; _ } -> fun () -> value
    | W.App { f ; x ; _ } ->
      let f = blocking_evaluator db f in
      let x = blocking_evaluator db x in
      fun () -> (f ()) (x ())
    | W.Both { fst ; snd ; _ } ->
      let fst = blocking_evaluator db fst in
      let snd = blocking_evaluator db snd in
      fun () -> (fst (), snd ())
    | W.Eval_path x ->
      let f = blocking_evaluator db x.workflow in
      fun () -> Db.path db (f ())
    | W.Select _ -> assert false
    | W.Input { path ; _ } -> fun () -> W.FS_path path
    | W.Value { id ; _ } ->
      fun () -> (load_value (Db.cache db id))
    | W.Path p -> fun () -> W.Cache_id p.id
    | W.Spawn s ->
      let elts = blocking_evaluator db s.elts in
      fun () ->
        let elts = elts () in
        List.init (List.length elts) ~f:(fun i -> blocking_evaluator db (s.f (W.list_nth s.elts i)) ())
    | W.Shell s -> fun () -> W.Cache_id s.id
    | W.List l ->
      let l = List.map l.elts ~f:(blocking_evaluator db) in
      fun () -> List.map l ~f:(fun f -> f())
    | W.List_nth l ->
      let elts = blocking_evaluator db l.elts in
      fun () ->
        let elts = elts () in
        List.nth_exn elts l.index
    | W.Glob g ->
      let dir = blocking_evaluator db g.dir in
      fun () ->
        let dir_path = dir () in
        Sys.readdir (Db.path db dir_path)
        |> Array.to_list
        |> List.map ~f:(fun fn -> W.Cd (dir_path, [fn]))

let rec shallow_eval
  : type s. _ t -> s W.t -> s Lwt.t
  = fun sched w ->
    match w with
    | W.Pure { value ; _ } -> Lwt.return value
    | W.App { f ; x ; _ } ->
      lwt_both (shallow_eval sched f) (shallow_eval sched x) >>= fun (f, x) ->
      let y = f x in
      Lwt.return y
    | W.Both { fst ; snd ; _ } ->
      lwt_both (shallow_eval sched fst) (shallow_eval sched snd) >>= fun (fst, snd) ->
      Lwt.return (fst, snd)
    | W.Eval_path w ->
      shallow_eval sched w.workflow >|= Db.path sched.db
    | W.Select s ->
      shallow_eval sched s.dir >>= fun dir ->
      Lwt.return (W.Cd (dir, s.sel))
    | W.Input { path ; _ } -> Lwt.return (W.FS_path path)
    | W.Value { id ; _ } ->
      Lwt.return (load_value (Db.cache sched.db id)) (* FIXME: blocking call *)
    | W.Spawn s -> (* FIXME: much room for improvement *)
      shallow_eval sched s.elts >>= fun elts ->
      let targets = List.init (List.length elts) ~f:(fun i -> s.f (W.list_nth s.elts i)) in
      Lwt_list.map_p (shallow_eval sched) targets
    | W.Path s -> Lwt.return (W.Cache_id s.id)
    | W.Shell s -> Lwt.return (W.Cache_id s.id)
    | W.List l ->
      Lwt_list.map_p (shallow_eval sched) l.elts
    | W.List_nth l ->
      shallow_eval sched l.elts >>= fun elts ->
      Lwt.return (List.nth_exn elts l.index)
    | W.Glob g ->
      shallow_eval sched g.dir >>= fun p ->
      Db.path sched.db p |>
      Misc.files_in_dir >|= fun files ->
      List.map files ~f:(fun fn -> W.Cd (p, [fn]))

and shallow_eval_command sched =
  let list xs = Lwt_list.map_p (shallow_eval_command sched) xs in
  let open Command in
  function
  | Simple_command cmd ->
    shallow_eval_template sched cmd >|= fun cmd ->
    Simple_command cmd
  | And_list xs ->
    list xs >|= fun xs -> And_list xs
  | Or_list xs ->
    list xs >|= fun xs -> Or_list xs
  | Pipe_list xs ->
    list xs >|= fun xs -> Pipe_list xs
  | Docker (env, cmd) ->
    shallow_eval_command sched cmd >|= fun cmd ->
    Docker (env, cmd)

and shallow_eval_template sched toks =
    Lwt_list.map_p (shallow_eval_token sched) toks

and shallow_eval_token sched =
  let open Template in
  function
  | D (Workflow.Path_token w) -> shallow_eval sched w >|= fun p -> D (Execution_env.Path p)
  | D (Workflow.String_token w) -> shallow_eval sched w >|= fun p -> D (Execution_env.String p)
  | F f -> shallow_eval_template sched f >|= fun t -> F t
  | DEST | TMP | NP | MEM | S _ as tok -> Lwt.return tok

let register_build sched ~id ~build_trace =
  let open Eval_thread.Infix in
  (
    match Table.find sched.traces id with
    | None ->
      let trace = build_trace () in
      Table.set sched.traces ~key:id ~data:trace ;
      trace
    | Some trace -> trace
  ) >>= fun trace ->
  if Execution_trace.is_errored trace then
    Eval_thread.fail1 trace
  else
    Lwt_result.return trace

let np_requirement
  : type s. s Workflow.t -> int
  = function
    | Pure _ -> 0
    | App _  -> 0
    | Spawn _ -> 0
    | Both _ -> 0
    | Eval_path _ -> 0
    | Input _ -> 0
    | Select _ -> 0
    | List _ -> 0
    | List_nth _ -> 0
    | Glob _ -> 0
    | Value x -> x.np
    | Path x -> x.np
    | Shell x -> x.np

let mem_requirement
  : type u v. u t -> v Workflow.t -> int Lwt.t
  = fun sched -> function
    | Pure _ -> Lwt.return 0
    | App _  -> Lwt.return 0
    | Spawn _ -> Lwt.return 0
    | Both _ -> Lwt.return 0
    | Eval_path _ -> Lwt.return 0
    | Input _ -> Lwt.return 0
    | Select _ -> Lwt.return 0
    | List _ -> Lwt.return 0
    | List_nth _ -> Lwt.return 0
    | Glob _ -> Lwt.return 0
    | Value x -> shallow_eval sched x.mem
    | Path x -> shallow_eval sched x.mem
    | Shell x -> shallow_eval sched x.mem

let build_trace sched w perform =
  let ready = Unix.gettimeofday () in
  log ~time:ready sched (Logger.Workflow_ready w) ;
  mem_requirement sched w >>= fun mem ->
  let requirement = Allocator.Request { np = np_requirement w ; mem } in
  Allocator.request sched.allocator requirement >>= function
  | Ok resource ->
    let open Eval_thread.Infix in
    let start = Unix.gettimeofday () in
    log ~time:start sched (Logger.Workflow_started (w, resource)) ;
    perform resource >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    log ~time:_end_ sched (Logger.Workflow_ended { outcome ; start ; _end_ }) ;
    Allocator.release sched.allocator resource ;
    Eval_thread.return (
      Execution_trace.Run { ready ; start  ; _end_ ; outcome }
    )
  | Error (`Msg msg) ->
    log sched (Logger.Workflow_allocation_error (w, msg)) ;
    Eval_thread.return (Execution_trace.Allocation_error { id = Workflow.id w ; msg })

let cached_build sched ~id ~f =
  if Sys.file_exists (Db.cache sched.db id) = `Yes
  then Eval_thread.return (Execution_trace.Done_already { id })
  else f ()

let signal_trace_to_gc sched w t =
  if not (Execution_trace.is_errored t) then (
    Maybe_gc.tag_as_built sched.gc w
  )

let schedule_cached_workflow sched ~id w ~deps ~perform =
  let open Eval_thread.Infix in
  register_build sched ~id ~build_trace:(fun () ->
      cached_build sched ~id ~f:(fun () ->
          deps () >>= fun () ->
          build_trace sched w perform
        )
    )
  >>| signal_trace_to_gc sched w
  |> Eval_thread.ignore

let rec build
  : type u v. _ t -> ?target:v W.t -> u W.t -> unit thread
  = fun sched ?target w ->
    let open Eval_thread.Infix in
    match w with
    | W.Pure _ -> Eval_thread.return ()
    | W.App { x ; f ; _ } ->
      Eval_thread.both (build sched ?target x) (build sched ?target f)
      >>| ignore
    | W.Both { fst ; snd ; _ } ->
      Eval_thread.both (build sched ?target fst) (build sched ?target snd)
      >>| ignore
    | W.Eval_path { workflow ; _ } -> build sched ?target workflow
    | List_nth l -> build sched ?target l.elts
    | Glob g -> build sched ?target g.dir
    | W.Spawn { elts ; f ; _ } ->
      build sched ?target elts >>= fun () ->
      shallow_eval sched elts >> fun elts_value ->
      let n = List.length elts_value in
      let targets = List.init n ~f:(fun i -> f (W.list_nth elts i)) in
      Lwt_list.iter_p (Maybe_gc.register ?target sched.gc) targets >> fun () ->
      Eval_thread.join ~f:(build ?target sched) targets
    | W.Input { id ; path ; _ } ->
      register_build sched ~id ~build_trace:(fun () ->
          build_trace sched w (fun _ -> perform_input sched ~id ~path)
        )
      |> Eval_thread.ignore

    | W.Select { id ; dir ; sel ; _ } ->
      build sched ?target dir >>= fun () ->
      shallow_eval sched dir >> fun dir ->
      register_build sched ~id ~build_trace:(fun () ->
          build_trace sched w (fun _ -> perform_select sched ~id ~dir ~sel)
        )
      |> Eval_thread.ignore

    | W.Value { task = workflow ; id ; descr ; _ } ->
      schedule_cached_workflow sched ~id w
        ~deps:(fun () -> build sched ~target:w workflow)
        ~perform:(fun _ ->
          let evaluator = blocking_evaluator sched.db workflow in
          worker (fun () ->
              let y = evaluator () () in
              save_value ~data:y (Db.cache sched.db id)
            ) () >|=
          function
          | Ok () ->
            Ok (Task_result.Plugin { id ; outcome = `Succeeded ; msg = None ; descr })
          | Error msg -> Ok (Task_result.Plugin { id ; outcome = `Failed ; msg = Some msg ; descr })
        )

    | W.Path { id ; task = workflow ; descr ; _ } ->
      schedule_cached_workflow sched ~id w
        ~deps:(fun () -> build sched ~target:w workflow)
        ~perform:(fun _ ->
          let evaluator = blocking_evaluator sched.db workflow in
          (* let env = *) (* FIXME: use this *)
          (*   Execution_env.make *)
          (*     ~use_docker:sched.use_docker *)
          (*     ~db:sched.db *)
          (*     ~np ~mem ~id *)
          (* in *)
          worker (Fn.flip evaluator (Db.cache sched.db id)) () >|=
          function
          | Ok () ->
            Ok (Task_result.Plugin { id ; outcome = `Succeeded ; msg = None ; descr })
          | Error msg -> Ok (Task_result.Plugin { id ; outcome = `Failed ; msg = Some msg ; descr })
        )

    | W.Shell { id ; task ; descr ; deps ; _ } ->
      schedule_cached_workflow sched ~id w
        ~deps:(fun () -> Eval_thread.join deps ~f:(fun (W.Any x) -> build sched ~target:w x))
        ~perform:(fun resource ->
            shallow_eval_command sched task >> fun cmd ->
            perform_shell sched resource ~id ~descr cmd >>= fun r ->
            Eval_thread.return r)

    | List l ->
      Eval_thread.join l.elts ~f:(build ?target sched)

let run sched =
  Maybe_gc.register sched.gc sched.target >>= fun () ->
  build sched sched.target
  >>= (fun r -> Maybe_gc.stop sched.gc >|= fun () -> r)
  |> Fn.flip Lwt_result.bind Lwt.(fun () -> shallow_eval sched sched.target >|= Result.return)
  |> Lwt_result.map_err Traces.elements

let eval ?np ?mem ?use_docker ?loggers ?collect db w =
  let sched = create ?np ?mem ?use_docker ?loggers ?collect db w in
  run sched

let error_report db traces =
  let buf = Buffer.create 1024 in
  List.iter traces ~f:(fun trace ->
      Execution_trace.error_report trace db buf
    ) ;
  Buffer.contents buf

let eval_exn ?np ?mem ?use_docker ?loggers ?collect db w =
  eval ?np ?mem ?use_docker ?loggers ?collect db w >|= function
  | Ok r -> r
  | Error errors -> failwith (error_report db errors)
