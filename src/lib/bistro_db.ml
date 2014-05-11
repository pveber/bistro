open Core.Std

module type Thread = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val mkdir_p : string -> unit t
  (** Equivalent of [mkdir -p] *)

  val echo : path:string -> string -> unit t
  (** [echo ~path msg] should append the string [msg] and a newline
      character to the file at location [path], creating it if it does
      not exist.

      @raise Invalid_argument if no file can be created at path
      [path] *)
end

module Blocking_thread = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let fail x = raise x
  let catch f g =
    try f ()
    with exn -> g exn

  let mkdir_p x = Unix.mkdir_p x
  let echo ~path msg =
    Out_channel.with_file ~append:true path ~f:(Fn.flip output_string msg)
end

module Make(T : Thread) = struct
  let ( >>= ) = T.bind

  type t = string

  let make x = x

  let cache_dir base = Filename.concat base "cache"
  let build_dir base = Filename.concat base "build"
  let tmp_dir base = Filename.concat base "tmp"
  let stderr_dir base = Filename.concat base "stderr"
  let stdout_dir base = Filename.concat base "stdout"
  let log_dir base = Filename.concat base "logs"
  let history_dir base = Filename.concat base "history"

  let init base =
    if Sys.file_exists_exn base then T.return `File_exists
    else (
      T.mkdir_p (tmp_dir base) >>= fun () ->
      T.mkdir_p (build_dir base) >>= fun () ->
      T.mkdir_p (cache_dir base) >>= fun () ->
      T.mkdir_p (stderr_dir base) >>= fun () ->
      T.mkdir_p (stdout_dir base) >>= fun () ->
      T.mkdir_p (log_dir base) >>= fun () ->
      T.mkdir_p (history_dir base) >>= fun () ->
      T.return `Ok
    )

  let aux_path f db w =
    Filename.concat (f db) (Bistro_workflow.digest w)

  let log_path db w = aux_path log_dir db w
  let build_path db w = aux_path build_dir db w
  let tmp_path db w = aux_path tmp_dir db w
  let stdout_path db w = aux_path stdout_dir db w
  let stderr_path db w = aux_path stderr_dir db w

  let rec path db = Bistro_workflow.(function
      | Input p -> p
      | Select (dir, p) ->
        Filename.concat (path db dir) p
      | Rule r as w -> aux_path cache_dir db w
    )

  module Log_msg = struct
    type level = [ `debug | `info | `warning | `error ]
    type t = Bistro_workflow.u option * level * Core.Time.t * string

    let make ?w level fmt =
      let open Printf in
      let f msg = w, level, Core.Time.now (), msg in
      ksprintf f fmt

    let string_of_level = function
      | `info -> "INFO"
      | `debug -> "DEBUG"
      | `warning -> "WARNING"
      | `error -> "ERROR"

    let to_string (_,level,t,msg) =
      let open Unix in
      sprintf
        "[%s][%s] %s"
        (string_of_level level) (Time.format t "%Y-%m-%d %H:%M:%S") msg
  end

  let log ?hook db ?w level fmt =
    let msg = Log_msg.make ?w level fmt in
    let hook_thread = match hook with
      | None -> T.return ()
      | Some f -> f msg
    in
    hook_thread >>= fun () ->
    T.echo (assert false) (Log_msg.to_string msg)

end

include Make(Blocking_thread)
