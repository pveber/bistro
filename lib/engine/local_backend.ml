open Core
open Lwt.Infix
open Bistro_internals

type t = {
  logger : Logger.t ;
  db : Db.t ;
  allocator : Allocator.t ;
}

type token = unit

let create
    ?(np = 1) ?mem:(`GB mem = `GB 1)
    ?(loggers = [])
    db
  =
  let logger = Logger.tee loggers in
  { logger ; db ;
    allocator = Allocator.create ~np ~mem:(mem * 1024) }

let log ?(time = Unix.gettimeofday ()) backend event =
  backend.logger#event backend.db time event

let run_shell_command _ () = Shell_command.run

let eval _ () f x =
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

let build_trace backend w requirement perform =
  let ready = Unix.gettimeofday () in
  log ~time:ready backend (Logger.Workflow_ready w) ;
  Allocator.request backend.allocator requirement >>= function
  | Ok resource ->
    let open Eval_thread.Infix in
    let start = Unix.gettimeofday () in
    log ~time:start backend (Logger.Workflow_started (w, resource)) ;
    perform () resource >>= fun outcome ->
    let _end_ = Unix.gettimeofday () in
    log ~time:_end_ backend (Logger.Workflow_ended { outcome ; start ; _end_ }) ;
    Allocator.release backend.allocator resource ;
    Eval_thread.return (
      Execution_trace.Run { ready ; start  ; _end_ ; outcome }
    )
  | Error (`Msg msg) ->
    log backend (Logger.Workflow_allocation_error (w, msg)) ;
    Eval_thread.return (Execution_trace.Allocation_error { id = Workflow.id w ; msg })
