open Core
open Lwt.Infix
open Bistro_internals
module W = Bistro_internals.Workflow

type error = [
  | `Msg of string
]

type config = {
  db : Db.t ;
  use_docker : bool ;
}

let db = Db.init_exn "_bistro" (* FIXME *)
let config = { db ; use_docker = true }

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

let lwt_both x y =
  x >>= fun x ->
  y >>= fun y ->
  Lwt.return (x, y)

let list_nth xs i =
  W.(pure ~id:"List.nth" List.nth_exn $ xs $ int i)

let step_outcome ~exit_code ~dest_exists=
  match exit_code, dest_exists with
    0, true -> `Succeeded
  | 0, false -> `Missing_output
  | _ -> `Failed

let perform_shell id cmd =
  let env =
    Execution_env.make
      ~use_docker:config.use_docker
      ~db:config.db
      ~np:1 ~mem:0 ~id
  in
  let cmd = Shell_command.make env cmd in
  Shell_command.run cmd >>= fun (exit_code, dest_exists) ->
  let cache_dest = Db.cache config.db id in
  let outcome = step_outcome ~exit_code ~dest_exists in
  Misc.(
    if outcome = `Succeeded then
      mv env.dest cache_dest >>= fun () ->
      remove_if_exists env.tmp_dir
    else
      Lwt.return ()
  ) >>= fun () ->
  Lwt.return ((*Task_result.Shell {
      outcome ;
      id ;
      descr ;
      exit_code ;
      cmd = Shell_command.text cmd ;
      file_dumps = Shell_command.file_dumps cmd ;
      cache = if outcome = `Succeeded then Some cache_dest else None ;
      stdout = env.stdout ;
      stderr = env.stderr ;
                }*)())

let rec blocking_evaluator : type s. s W.t -> (unit -> s) = function
  | W.Pure { value ; _ } -> fun () -> value
  | W.App { f ; x ; _ } ->
    let f = blocking_evaluator f in
    let x = blocking_evaluator x in
    fun () -> (f ()) (x ())
  | W.Both { fst ; snd ; _ } ->
    let fst = blocking_evaluator fst in
    let snd = blocking_evaluator snd in
    fun () -> (fst (), snd ())
  | W.Eval_path x ->
    let f = blocking_evaluator x.workflow in
    fun () -> Db.path db (f ())
  | W.Select _ -> assert false
  | W.Input { path ; _ } -> fun () -> W.FS_path path
  | W.Value { id ; _ } ->
    fun () -> (load_value (Db.cache db id))
  | W.Path _ -> assert false
  | W.Spawn _ -> assert false
  | W.Shell s -> fun () -> W.Cache_id s.id

let rec shallow_eval : type s. s W.t -> s Lwt.t = function
  | W.Pure { value ; _ } -> Lwt.return value
  | W.App { f ; x ; _ } ->
    lwt_both (shallow_eval f) (shallow_eval x) >>= fun (f, x) ->
    let y = f x in
    Lwt.return y
  | W.Both { fst ; snd ; _ } ->
    lwt_both (shallow_eval fst) (shallow_eval snd) >>= fun (fst, snd) ->
    Lwt.return (fst, snd)
  | W.Eval_path w ->
    shallow_eval w.workflow >|= Db.path db
  | W.Select s ->
    shallow_eval s.dir >>= fun dir ->
    Lwt.return (W.Cd (dir, s.sel))
  | W.Input { path ; _ } -> Lwt.return (W.FS_path path)
  | W.Value { id ; _ } ->
    Lwt.return (load_value (Db.cache db id)) (* FIXME: blocking call *)
  | W.Spawn s -> (* FIXME: much room for improvement *)
    shallow_eval s.elts >>= fun elts ->
    let targets = List.init (List.length elts) ~f:(fun i -> s.f (list_nth s.elts i)) in
    Lwt_list.map_p shallow_eval targets
  | W.Path s -> Lwt.return (W.Cache_id s.id)
  | W.Shell s -> Lwt.return (W.Cache_id s.id)

and shallow_eval_command =
  let list xs = Lwt_list.map_p shallow_eval_command xs in
  let open Command in
  function
  | Simple_command cmd ->
    shallow_eval_template cmd >|= fun cmd ->
    Simple_command cmd
  | And_list xs ->
    list xs >|= fun xs -> And_list xs
  | Or_list xs ->
    list xs >|= fun xs -> Or_list xs
  | Pipe_list xs ->
    list xs >|= fun xs -> Pipe_list xs
  | Docker (env, cmd) ->
    shallow_eval_command cmd >|= fun cmd ->
    Docker (env, cmd)

and shallow_eval_template = fun toks ->
    Lwt_list.map_p shallow_eval_token toks

and shallow_eval_token =
  let open Template in
  function
  | D w -> shallow_eval w >|= fun p -> D p
  | F f -> shallow_eval_template f >|= fun t -> F t
  | DEST | TMP | NP | MEM | S _ as tok -> Lwt.return tok

let rec build : type s. s W.t -> unit Lwt.t = function
  | W.Pure _ -> Lwt.return ()
  | W.App { x ; f ; _ } ->
    lwt_both (build x) (build f) >|= ignore
  | W.Both { fst ; snd ; _ } ->
    lwt_both (build fst) (build snd) >|= ignore
  | W.Eval_path { workflow ; _ } -> build workflow
  | W.Spawn { elts ; f ; _ } ->
    build elts >>= fun () ->
    shallow_eval elts >>= fun elts_value ->
    let n = List.length elts_value in
    List.init n ~f:(fun i -> f (list_nth elts i))
    |> List.map ~f:build
    |> Lwt.join
  | W.Input _ -> Lwt.return () (* FIXME: check path *)
  | W.Select _ -> Lwt.return () (* FIXME: check path *)
  | W.Value { task = workflow ; id ; _ } ->
    if Sys.file_exists (Db.cache db id) = `Yes
    then Lwt.return ()
    else
      let () = printf "build %s\n" id in
      let evaluator = blocking_evaluator workflow in
      worker (fun () ->
          let y = evaluator () () in
          save_value ~data:y (Db.cache db id)
        ) ()
      >|= ignore (* FIXME *)
  | W.Path { id ; task = workflow ; _ } ->
    if Sys.file_exists (Db.cache db id) = `Yes
    then Lwt.return ()
    else
      let () = printf "build %s\n" id in
      let evaluator = blocking_evaluator workflow in
      (* let env = *) (* FIXME: use this *)
      (*   Execution_env.make *)
      (*     ~use_docker:config.use_docker *)
      (*     ~db:config.db *)
      (*     ~np ~mem ~id *)
      (* in *)
      worker (Fn.flip evaluator (Db.cache db id)) ()
      >|= ignore (* FIXME *)
  | W.Shell s ->
    if Sys.file_exists (Db.cache db s.id) = `Yes
    then Lwt.return ()
    else
      build_command_deps s.task >>= fun () ->
      shallow_eval_command s.task >>= fun cmd ->
      perform_shell s.id cmd

and build_command_deps
  : W.path W.t Command.t -> unit Lwt.t
  = function
    | Simple_command cmd -> build_template_deps cmd
    | And_list xs
    | Or_list xs
    | Pipe_list xs ->
      List.map ~f:build_command_deps xs
      |> Lwt.join
    | Docker (_, cmd) -> build_command_deps cmd

and build_template_deps
  : W.path W.t Template.t -> unit Lwt.t
  = fun toks ->
    List.map ~f:build_template_token_deps toks
    |> Lwt.join

and build_template_token_deps
  : W.path W.t Template.token -> unit Lwt.t
  = function
    | D w -> build w
    | F f -> build_template_deps f
    | DEST | TMP | NP | MEM | S _ -> Lwt.return ()

let eval : type s. s Bistro.workflow -> s Lwt.t =
  fun w ->
    let w = Bistro.Private.reveal w in
    build w >>= fun () ->
    shallow_eval w
