open Core
open Lwt.Infix
module W = Bistro_internals.Workflow

type error = [
  | `Msg of string
]

let db = Db.init_exn "_bistro" (* FIXME *)

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
  | W.Eval_path _ -> assert false
  | W.Select _ -> assert false
  | W.Input { path ; _ } -> fun () -> W.FS_path path
  | W.Value { id ; _ } ->
    fun () -> (load_value (Db.cache db id))
  | W.Path _ -> assert false
  | W.Spawn _ -> assert false
  | W.Shell _s ->
    assert false

let rec shallow_eval : type s. s W.t -> s Lwt.t = function
  | W.Pure { value ; _ } -> Lwt.return value
  | W.App { f ; x ; _ } ->
    lwt_both (shallow_eval f) (shallow_eval x) >>= fun (f, x) ->
    let y = f x in
    Lwt.return y
  | W.Both { fst ; snd ; _ } ->
    lwt_both (shallow_eval fst) (shallow_eval snd) >>= fun (fst, snd) ->
    Lwt.return (fst, snd)
  | W.Eval_path _ -> assert false
  | W.Select _ -> assert false
  | W.Input { path ; _ } -> Lwt.return (W.FS_path path)
  | W.Value { id ; _ } ->
    Lwt.return (load_value (Db.cache db id))
  | W.Path _ -> assert false
  | W.Spawn _ -> assert false
  | W.Shell _s ->
    assert false

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
  | W.Path _ -> assert false
  | W.Shell _s ->
    assert false

let eval : type s. s Bistro.workflow -> s Lwt.t =
  fun w ->
    let w = Bistro.Private.reveal w in
    build w >>= fun () ->
    shallow_eval w
