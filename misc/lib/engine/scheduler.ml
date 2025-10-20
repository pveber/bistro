open Core
open Lwt.Infix
open Bistro_internals

module W = Bistro_internals.Workflow
module Sys = Sys_unix
module Unix = Core_unix

type error = [
  | `Msg of string
]

module Table = String.Table

type 'a thread = 'a Eval_thread.t

let lwt_both x y =
  x >>= fun x ->
  y >>= fun y ->
  Lwt.return (x, y)

module Gc : sig
  type t
  val create : Db.t -> (Logger.event -> unit) -> t
  val register : t -> ?target:_ W.t -> _ W.t -> unit Lwt.t
  val tag_workflow_as_built : t -> _ W.t -> unit
  val uses_singularity_image : t -> _ W.t -> Workflow.container_image -> unit
  val stop : t -> unit Lwt.t
  val protect : t -> _ W.t -> unit
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
  module Elt = struct
    type t =
      | Workflow : W.any -> t
      | Singularity_image : Workflow.container_image -> t

    let workflow w = Workflow (W.Any w)
    let singularity_image x = Singularity_image x

    let id = function
      | Workflow w -> W.Any.id w
      | Singularity_image i -> Db.container_image_identifier i

    let compare x y =
      String.compare (id x) (id y)

    let equal x y =
      String.equal (id x) (id y)

    let hash x = Hashtbl.hash (id x)

    let path db = function
      | Workflow w -> Db.cache db (W.Any.id w)
      | Singularity_image i -> Db.singularity_image db i
  end

  module S = Stdlib.Set.Make(Elt)
  module T = struct
    include Stdlib.Hashtbl.Make(Elt)
    let update t ~key ~default ~f =
      let data = match find t key with
        | d -> d
        | exception Stdlib.Not_found -> default
      in
      replace t key (f data)

    let adj_add t u v =
      update t ~key:u ~default:S.empty ~f:(S.add v)

    let adj_find t u = match find t u with
      | x -> x
      | exception Stdlib.Not_found -> S.empty

    let incr_count t u =
      update t ~key:u ~default:0 ~f:succ

    let decr_count t u =
      let n = match find t u with
        | n -> n - 1
        | exception Stdlib.Not_found -> assert false
      in
      replace t u n ;
      n
  end


  type msg =
    | Built : Elt.t -> msg
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

  let update_counts_and_collect gc x =
    let n = T.decr_count gc.counts x in
    if n = 0 then (
      if not (S.mem x gc.protected) then (
        gc.log (
          match x with
          | Workflow (W.Any w) -> Logger.Workflow_collected w
          | Singularity_image i -> Logger.Singularity_image_collected i
        ) ;
        Misc.remove_if_exists (Elt.path gc.db x)
      )
      else
        Lwt.return ()
    )
    else Lwt.return ()

  let rec main gc =
    Lwt_queue.pop gc.inbox >>= function
    | Built x ->
      T.adj_find gc.depends_on x
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

  let tag_workflow_as_built gc w =
    Lwt_queue.push gc.inbox (Built (Elt.workflow w))

  let protect gc w =
    gc.protected <- S.add (Elt.workflow w) gc.protected

  let uses gc u v =
    match u with
    | None -> protect gc v
    | Some u ->
      let u = Elt.workflow u and v = Elt.workflow v in
      T.adj_add gc.depends_on u v ;
      T.adj_add gc.is_used_by v u ;
      T.incr_count gc.counts v

  let uses_singularity_image gc u v =
    let u = Elt.workflow u and v = Elt.singularity_image v in
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
    | Plugin { task = Value_plugin v ; _ } ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else register gc ~target:w v
    | Plugin { task = Path_plugin p ; _ } ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else register gc ~target:w p
    | Shell s ->
      uses gc target w ;
      if stop_register gc w then Lwt.return ()
      else Lwt_list.iter_p (register_any gc ~target:w) s.deps
    | Glob g ->
      register gc ?target g.dir
    | Trywith tw ->
      register gc ?target tw.w >>= fun () ->
      register gc ?target tw.failsafe
    | Ifelse ie ->
      register gc ?target ie.cond >>= fun () ->
      register gc ?target ie._then_ >>= fun () ->
      register gc ?target ie._else_

  and register_any : type u. t -> ?target:u W.t -> W.any -> unit Lwt.t = fun gc ?target (Workflow.Any w) ->
    register gc ?target w

  and stop_register : type u. t -> u W.t -> bool = fun gc w ->
    let u = Elt.workflow w in
    T.mem gc.depends_on u || Db.is_in_cache gc.db (W.Any w)

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
      |> Seq.flat_map (fun (u, s) ->
          match u with
          | Elt.Workflow w_u ->
            Seq.filter_map
              (function
                | Elt.Workflow w_v -> Some (w_u, w_v)
                | Singularity_image _ -> None)
              (S.to_seq s)
          | Singularity_image _ -> Seq.empty
        )
      |> Stdlib.List.of_seq ;
    protected =
      S.to_seq gc.protected
      |> Seq.filter_map (function Elt.Workflow w -> Some w | Singularity_image _ -> None)
      |> Stdlib.List.of_seq ;
  }

end

module Maybe_gc : sig
  type t = Gc.t option
  val register : t -> ?target:_ W.t -> _ W.t -> unit Lwt.t
  val uses_singularity_image : t -> _ W.t -> Workflow.container_image -> unit
  val tag_workflow_as_built : t -> _ W.t -> unit
  val stop : t -> unit Lwt.t
  val protect : t -> _ W.t -> unit
end
=
struct
  type t = Gc.t option
  let register o ?target w = match o with
    | Some gc -> Gc.register gc ?target w
    | None -> Lwt.return ()

  let uses_singularity_image o w s = match o with
    | Some gc -> Gc.uses_singularity_image gc w s
    | None -> ()

  let tag_workflow_as_built o w = match o with
    | Some gc -> Gc.tag_workflow_as_built gc w
    | None -> ()

  let stop = function
    | Some gc -> Gc.stop gc
    | None -> Lwt.return ()

  let protect o w =
    match o with
    | Some gc -> Gc.protect gc w
    | None -> ()
end

module Synchro : sig
  type 'a t
  val create : unit -> 'a t
  val signal : 'a t -> 'a -> unit
  val wait : 'a t -> 'a Lwt.t
end
=
struct
  type 'a t = ('a Lwt.t * 'a Lwt.u)
  let create () = Lwt.wait ()
  let signal (_, u) x = Lwt.wakeup u x
  let wait = fst
end

module type Backend = sig
  open Bistro_internals

  type t
  type token

  val run_shell_command :
    t ->
    token ->
    Shell_command.t ->
    (int * bool, string) Lwt_result.t

  val eval :
    t ->
    token ->
    ('a -> unit) ->
    'a ->
    (unit, string) Lwt_result.t

  val build_trace :
    t ->
    _ Workflow.t ->
    Allocator.request ->
    (token -> Allocator.resource -> Execution_trace.Run_details.t Eval_thread.t) ->
    Execution_trace.t Eval_thread.t

  val stop : t -> unit Lwt.t
end

module Make(Backend : Backend) = struct

  type t = {
    start : unit Synchro.t ;
    _end_ : unit Synchro.t ;
    closed : bool ;
    db : Db.t ;
    logger : Logger.t ;
    allowed_containers : [`Docker | `Singularity] list ;
    traces : Execution_trace.t thread Table.t ;
    gc : Gc.t option ;
    backend : Backend.t ;
    allocator : Allocator.t ;
  }

  let create
      ?(allowed_containers = [`Docker])
      ?(loggers = [])
      ?(collect = false)
      backend
      db =
    let allocator = Allocator.create ~np:1 ~mem:0 in
    let logger = Logger.tee loggers in
    {
      start = Synchro.create () ;
      _end_ = Synchro.create () ;
      closed = false ;
      db ;
      allowed_containers ;
      traces = String.Table.create () ;
      logger ;
      gc =
        if collect then
          let gc_log event = logger#event db (Unix.gettimeofday ()) event in
          Some (Gc.create db gc_log)
        else None ;
      backend ;
      allocator ;
    }

  let gc_state sched = Option.map ~f:Gc.state sched.gc

  let protect sched w =
    Maybe_gc.protect sched.gc (Bistro.Private.reveal w)

  (* let log ?(time = Unix.gettimeofday ()) sched event =
   *   sched.logger#event sched.db time event *)

  let perform_input ~path ~id =
    let pass = match Sys.file_exists path with `Yes -> true | `Unknown | `No -> false in
    (* (
     *   if pass then Misc.cp path (Db.cache sched.db id)
     *   else Lwt.return ()
     * ) >>= fun () -> *)
    Eval_thread.return (
      Execution_trace.Run_details.Input { id ; pass ; path }
    )

  let perform_select ~db ~id ~dir ~sel =
    let p = Filename.concat (Db.path db dir) (Path.to_string sel) in
    let pass = match Sys.file_exists p with `Yes -> true | `Unknown | `No -> false in
    Eval_thread.return (
      Execution_trace.Run_details.Select {
        id ;
        pass ;
        dir_path = Db.path db dir ;
        sel ;
      }
    )

  let step_outcome ~exit_code ~dest_exists=
    match exit_code, dest_exists with
      0, true -> `Succeeded
    | 0, false -> `Missing_output
    | i, _ -> `Error_exit_code i

  let perform_shell { backend ; allowed_containers ; db ; _ } token (Allocator.Resource { np ; mem }) ~id ~descr images cmd =
    let env =
      Execution_env.make
        ~allowed_containers
        ~db
        ~np ~mem ~id
    in
    let cmd = Shell_command.make env images cmd in
    Backend.run_shell_command backend token cmd >>= fun result ->
    let cache_dest = Db.cache db id in
    let outcome =
      match result with
      | Ok (exit_code, dest_exists) ->
        step_outcome ~exit_code ~dest_exists
      | Error msg -> `Scheduler_error msg
    in
    Misc.(
      if Execution_trace.Outcome.is_success outcome then
        mv env.dest cache_dest >>= fun () ->
        remove_if_exists env.tmp_dir
      else Lwt.return ()
    ) >>= fun () ->
    Eval_thread.return (Execution_trace.Run_details.Shell {
        outcome ;
        id ;
        descr ;
        cmd = Shell_command.text cmd ;
        file_dumps = Shell_command.file_dumps cmd ;
        cache = (
          match outcome with
          | `Succeeded -> Some cache_dest
          | `Plugin_failure _
          | `Missing_output
          | `Error_exit_code _
          | `Scheduler_error _ -> None) ;
        stdout = env.stdout ;
        stderr = env.stderr ;
      })

  let perform_plugin { backend ; db ; _ } token (Allocator.Resource _) ~id ~descr f =
    Backend.eval backend token (fun () ->
        let y = f () in
        Misc.save_value ~data:y (Db.cache db id)
      ) () >|=
    function
    | Ok () ->
      Ok (Execution_trace.Run_details.Plugin { id ; outcome = `Succeeded ; descr })
    | Error msg -> Ok (Execution_trace.Run_details.Plugin { id ; outcome = `Plugin_failure msg ; descr })

  let perform_path_plugin { db ; backend ; _ } token (Allocator.Resource { mem ; np }) ~id ~descr f =
    let env =
      Execution_env.make
        ~allowed_containers:[]
        ~db
        ~np ~mem ~id
    in
    let cache_dest = Db.cache db id in
    Misc.remove_if_exists env.tmp_dir >>= fun () ->
    Unix.mkdir_p env.tmp ;
    Backend.eval backend token f env.dest >>= function
    | Ok () ->
      let outcome =
        match Sys.file_exists env.dest with
        | `Yes -> `Succeeded
        | `Unknown | `No -> `Missing_output
      in
      Misc.(
        match outcome with
        | `Succeeded ->
          mv env.dest cache_dest >>= fun () ->
          remove_if_exists env.tmp_dir
        | `Missing_output -> Lwt.return ()
      ) >>= fun () ->
      Lwt_result.return (Execution_trace.Run_details.Plugin { id ; outcome ; descr })
    | Error msg ->
      Lwt_result.return (Execution_trace.Run_details.Plugin { id ; outcome = `Plugin_failure msg ; descr })

  let run_trywith_recovery (rd : Execution_trace.Run_details.t) =
    match rd with
    | Input i -> not i.pass
    | Select s -> not s.pass
    | Container_image_fetch _ -> false
    | Shell { outcome ; _ }
    | Plugin { outcome ; _ } -> (
        match outcome with
        | `Error_exit_code _
        | `Missing_output
        | `Plugin_failure _ -> true
        | `Succeeded
        | `Scheduler_error _ -> false
      )


  let rec delayed_eval
    : type s. t -> s W.t -> (unit -> s option) Lwt.t
    = fun sched w ->
      match w with
      | W.Pure { value ; _ } -> Lwt.return (fun () -> Some value)
      | W.App { f ; x ; _ } ->
        lwt_both (delayed_eval sched f) (delayed_eval sched x) >|= fun (f, x) ->
        fun () -> Option.(
            f () >>= fun f ->
            x () >>| fun x ->
            f x)
      | W.Both { fst ; snd ; _ } ->
        lwt_both (delayed_eval sched fst) (delayed_eval sched snd) >|= fun (fst, snd) ->
        fun () -> Option.(
            fst () >>= fun fst ->
            snd () >>| fun snd ->
            (fst, snd)
          )
      | W.Eval_path w ->
        delayed_eval sched w.workflow >|= fun f ->
        fun () -> Option.map (f ()) ~f:(Db.path sched.db)
      | W.Select s ->
        delayed_eval sched s.dir >|= fun dir ->
        fun () -> Option.map (dir ()) ~f:(fun d -> W.cd d s.sel)
      | W.Input { path ; _ } ->
        Lwt.return (fun () -> Some (W.FS_path (Misc.absolutize path)))
      | W.Spawn s -> ( (* FIXME: much room for improvement *)
          delayed_eval sched s.elts >>= fun elts ->
          match elts () with (* FIXME: blocking call *)
          | Some elts ->
            let targets = List.init (List.length elts) ~f:(fun i -> s.f (W.list_nth s.elts i)) in
            Lwt_list.map_p (delayed_eval sched) targets >>= fun evaluators ->
            Lwt.return (fun () -> List.map evaluators ~f:(fun f -> f ()) |> Option.all)
          | None -> Lwt.return (Fn.const None)
        )
      | W.Plugin { id ; task = Value_plugin _ ; _ } ->
        Lwt.return (
          let db = sched.db in
          fun () -> (
            let cache_path = Db.cache db id in
            match Sys.file_exists cache_path with
            | `Yes -> Some (Misc.load_value cache_path)
            | `No | `Unknown -> None
          )
        )
      | W.Plugin { id ; task = Path_plugin _ ; _ } ->
        Lwt.return (
          let db = sched.db in
          fun () -> (
            let cache_path = Db.cache db id in
            match Sys.file_exists cache_path with
            | `Yes -> Some (W.Cache_id id)
            | `No | `Unknown -> None
          )
        )
      | W.Shell s ->
        Lwt.return (
          let db = sched.db in
          let id = s.id in
          fun () -> (
            let cache_path = Db.cache db id in
            match Sys.file_exists cache_path with
            | `Yes -> Some (W.Cache_id id)
            | `No | `Unknown -> None
          )
        )
      | W.List l ->
        Lwt_list.map_p (delayed_eval sched) l.elts >>= fun evaluators ->
        Lwt.return (
          fun () -> List.map evaluators ~f:(fun f -> f ()) |> Option.all
        )
      | W.List_nth l ->
        let i = l.index in
        delayed_eval sched l.elts >>= fun elts ->
        Lwt.return (fun () -> Option.map (elts ()) ~f:(fun xs -> List.nth_exn xs i))
      | W.Glob { dir ; type_selection ; pattern ; id = _ } ->
        delayed_eval sched dir >>= fun dir ->
        Lwt.return (
          fun () ->
            Option.map (dir ()) ~f:(fun p ->
                Db.path sched.db p |> fun dir_path ->
                match Misc.glob ~type_selection ~pattern dir_path with
                | Error (`Msg s) -> failwithf "glob error: %s" s ()
                | Ok xs ->
                  List.map xs ~f:(fun fn -> W.FS_path fn)
              )
        )
      | Trywith tw -> (
          match Hashtbl.find sched.traces (Workflow.id tw.w) with
          | Some eventual_trace -> (
              eventual_trace >>= function
              | Ok (Run r) ->
                if run_trywith_recovery r.details then
                  delayed_eval sched tw.failsafe
                else if Execution_trace.Run_details.succeeded r.details then
                  delayed_eval sched tw.w
                else
                  Lwt.return (Fn.const None)
              | Ok (Done_already _) -> delayed_eval sched tw.w
              | Ok (Canceled _ | Allocation_error _) ->
                Lwt.return (Fn.const None)
              | Error _ ->
                delayed_eval sched tw.failsafe
            )
          | None -> assert false (* delayed_eval should not be called
                                    on workflow that has not been
                                    successfully built *)
        )
      | Ifelse ie -> (
          delayed_eval sched ie.cond >>= fun cond ->
          match cond () with
          | Some true -> delayed_eval sched ie._then_
          | Some false -> delayed_eval sched ie._else_
          | None -> Lwt.return (fun () -> None)
        )

  let shallow_eval sched w =
    delayed_eval sched w >>= fun f ->
    Lwt.return (f ())

  let shallow_eval_exn ~msg sched w =
    shallow_eval sched w >|= fun r ->
    Option.value_exn ~message:("shallow_eval_exn:" ^ msg) r

  let rec shallow_eval_command sched =
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

  and shallow_eval_template sched toks =
    Lwt_list.map_p (shallow_eval_token sched) toks

  and shallow_eval_token sched =
    let open Template in
    function
    | D (Workflow.Path_token w) -> shallow_eval_exn sched ~msg:"token D" w >|= fun p -> D (Execution_env.Path p)
    | D (Workflow.Path_list_token { elts ; quote ; sep }) ->
      shallow_eval_exn sched elts  ~msg:"token Ds" >|= fun elts -> D (Execution_env.Path_list { elts ; quote ; sep })
    | D (Workflow.String_token w) -> shallow_eval_exn sched w ~msg:"token S" >|= fun p -> D (Execution_env.String p)
    | F f -> shallow_eval_template sched f >|= fun t -> F t
    | DEST | TMP | NP | MEM | S _ as tok -> Lwt.return tok

  let register_build sched ~id ~build_trace =
    let open Eval_thread.Infix in
    (
      match Hashtbl.find sched.traces id with
      | None ->
        let trace = build_trace () in
        Hashtbl.set sched.traces ~key:id ~data:trace ;
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
      | Plugin x -> x.np
      | Shell x -> x.np
      | Trywith _ -> 0
      | Ifelse _ -> 0

  let opt_mem_requirement sched = function
    | None -> Lwt.return 100
    | Some mem -> shallow_eval_exn  ~msg:"opt mem" sched mem

  let mem_requirement
    : type u. t -> u Workflow.t -> int Lwt.t
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
      | Plugin x -> opt_mem_requirement sched x.mem
      | Shell x -> opt_mem_requirement sched x.mem
      | Trywith _ -> Lwt.return 0
      | Ifelse _ -> Lwt.return 0

  let build_trace sched w perform =
    mem_requirement sched w >>= fun mem ->
    let requirement = Allocator.Request { np = np_requirement w ; mem } in
    Backend.build_trace sched.backend w requirement perform

  let cached_build sched ~id ~f =
    match Sys.file_exists (Db.cache sched.db id) with
    | `Yes -> Eval_thread.return (Execution_trace.Done_already { id })
    | `Unknown | `No -> f ()

  let signal_trace_to_gc sched w t =
    if not (Execution_trace.is_errored t) then (
      Maybe_gc.tag_workflow_as_built sched.gc w
    )

  let schedule_cached_workflow sched ~id w ~deps ~perform =
    let open Eval_thread.Infix in
    register_build sched ~id ~build_trace:(fun () ->
        cached_build sched ~id ~f:(fun () ->
            deps () >>= fun () ->
            build_trace sched w perform >|= fun trace_or_error ->
            (
              match trace_or_error with
              | Ok trace -> signal_trace_to_gc sched w trace
              | Error _ -> ()
            ) ;
            trace_or_error
          )
      )
    |> Eval_thread.ignore

  let schedule_container_image_fetch sched img =
    let id = Db.container_image_identifier img in
    let ready = Unix.gettimeofday () in
    register_build sched ~id ~build_trace:(fun () ->
        let dest = Db.singularity_image sched.db img in
        match Sys.file_exists dest with
        | `Yes ->
          Eval_thread.return (Execution_trace.Done_already { id })
        | `Unknown | `No -> (
            let req = Allocator.Request { np = 1 ; mem = 0 } in
            Allocator.request sched.allocator req >>= function
            | Ok resource ->
              let start = Unix.gettimeofday () in
              (* log ~time:start sched (Logger.Workflow_started (w, resource)) ; *)
              Singularity.fetch_image img dest >>= fun outcome ->
              let _end_ = Unix.gettimeofday () in
              Allocator.release sched.allocator resource ;
              Eval_thread.return @@ Execution_trace.Run {
                ready ; start ; _end_ ;
                details = Execution_trace.Run_details.Container_image_fetch { id ; outcome } ;
              }
            | Error _ ->
              assert false (* should never happen, we're asking so little here! *)
          )
      )
    |> Eval_thread.ignore

  let schedule_shell_container_image_fetch sched w (cmd : W.shell_command) =
    match Execution_env.choose_container sched.allowed_containers cmd.images with
    | Some (`Singularity_container i) ->
      Maybe_gc.uses_singularity_image sched.gc w i ;
      schedule_container_image_fetch sched i
    | Some (`Docker_container _) | None ->
      Eval_thread.return ()

  let rec build
    : type u v. t -> ?target:v W.t -> u W.t -> unit thread
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
        shallow_eval_exn  ~msg:"spawn"  sched elts >> fun elts_value ->
        let n = List.length elts_value in
        let targets = List.init n ~f:(fun i -> f (W.list_nth elts i)) in
        Lwt_list.iter_p (Maybe_gc.register ?target sched.gc) targets >> fun () ->
        Eval_thread.join ~f:(build ?target sched) targets
      | W.Input { id ; path ; _ } ->
        register_build sched ~id ~build_trace:(fun () ->
            build_trace sched w (fun _ _ -> perform_input ~id ~path)
          )
        |> Eval_thread.ignore

      | W.Select { id ; dir ; sel ; _ } ->
        build sched ?target dir >>= fun () ->
        shallow_eval_exn  ~msg:"select" sched dir >> fun dir ->
        register_build sched ~id ~build_trace:(fun () ->
            build_trace sched w (fun _ _ ->
                perform_select ~db:sched.db ~id ~dir ~sel
              )
          )
        |> Eval_thread.ignore

      | W.Plugin { task = Value_plugin workflow ; id ; descr ; _ } ->
        schedule_cached_workflow sched ~id w
          ~deps:(fun () -> build sched ~target:w workflow)
          ~perform:(fun token resource ->
              shallow_eval_exn ~msg:("plugin:" ^ descr) sched workflow >> fun f ->
              perform_plugin sched token resource ~id ~descr f
            )

      | W.Plugin { id ; task = Path_plugin workflow ; descr ; _ } ->
        schedule_cached_workflow sched ~id w
          ~deps:(fun () -> build sched ~target:w workflow)
          ~perform:(fun token resource ->
              shallow_eval_exn ~msg:"path_plugin" sched workflow >> fun f ->
              perform_path_plugin sched token resource ~id ~descr f
            )

      | W.Shell { id ; task ; descr ; deps ; _ } ->
        schedule_cached_workflow sched ~id w
          ~deps:Eval_thread.(fun () ->
              join2
                (schedule_shell_container_image_fetch sched w task)
                (join deps ~f:(fun (W.Any x) -> build sched ~target:w x)))
          ~perform:(fun token resource ->
              shallow_eval_command sched task.cmd >> fun cmd ->
              perform_shell sched token resource ~id ~descr task.images cmd >>= fun r ->
              Eval_thread.return r)

      | List l ->
        Eval_thread.join l.elts ~f:(build ?target sched)
      | Trywith tw -> (
          build sched ?target tw.w >> fun w_result ->
          match Hashtbl.find sched.traces (Workflow.id tw.w) with
          | Some eventual_trace -> (
              eventual_trace >> function
              | Ok (Run r) when run_trywith_recovery r.details ->
                build sched ?target tw.failsafe
              | Error _ ->
                build sched ?target tw.failsafe
              | _ -> Lwt.return w_result
            )
          | None -> assert false (* cannot happen since build has been called *)
        )
      | Ifelse ie -> (
          build sched ?target ie.cond >>= fun () ->
          shallow_eval_exn  ~msg:"ifelse" sched ie.cond >> fun cond ->
          if cond then build sched ?target ie._then_
          else build sched ?target ie._else_
        )

  let start sched = Synchro.signal sched.start ()

  let eval sched target =
    if sched.closed then failwith "Scheduler is closed" ;
    let target = Bistro.Private.reveal target in
    Synchro.wait sched.start >>= fun () ->
    Maybe_gc.register sched.gc target >>= fun () ->
    build sched target
    >>= (fun r -> Maybe_gc.stop sched.gc >|= fun () -> r) (* FIXME: is this the right moment?
                                                             what if eval is called several times? *)
    |> Fn.flip Lwt_result.bind Lwt.(fun () -> shallow_eval_exn ~msg:"eval" sched target >|= Result.return)
    |> Lwt_result.map_error Execution_trace.Set.elements

  let error_report { db ; _ } traces =
    let buf = Buffer.create 1024 in
    List.iter traces ~f:(fun trace ->
        Execution_trace.error_report trace db buf
      ) ;
    Buffer.contents buf

  let eval_exn sched w =
    eval sched w >|= function
    | Ok r -> r
    | Error errors -> failwith (error_report sched errors)

  let stop sched =
    Maybe_gc.stop sched.gc >>= fun () ->
    sched.logger#stop >>= fun () ->
    Backend.stop sched.backend
end

include Make(Local_backend)

let create
    ?np ?mem
    ?allowed_containers
    ?loggers
    ?collect db =
  let backend = Local_backend.create ?np ?mem ?loggers db in
  create ?allowed_containers ?loggers ?collect backend db

let simple_eval_exn
    ?np ?mem ?allowed_containers ?loggers
    ?collect ?(db_path = "_bistro") w =
  let db = Db.init_exn db_path in
  let sched = create ?np ?mem ?allowed_containers ?loggers ?collect db in
  let thread = eval_exn sched w in
  start sched ;
  Lwt_main.run thread

let build_file_exn ?np ?mem ?allowed_containers ?loggers ?collect ?db_path ~output file =
  let path =
    simple_eval_exn
      ?np ?mem ?loggers ?allowed_containers ?db_path ?collect
      (Bistro.Workflow.path file)
  in
  Lwt_main.run (Misc.cp path output)
