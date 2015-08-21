open Core_kernel.Std

let digest x =
  let buf = Buffer.create 253 in
  let format = Format.formatter_of_buffer buf in
  Print_code.print_code format x ;
  Digest.(to_hex (string (Buffer.contents buf)))

module Workflow : sig
  type 'a t
  val const   : 'a -> 'a t
  val return  : ?cached:bool -> 'a code -> 'a t
  val returnp : (string -> unit) code -> string t
  val bind    : 'a t -> ('a code -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a code -> 'b t) -> 'b t

  val code : 'a t -> 'a code
  val build_code : 'a t -> unit code
  val eval_code : 'a t -> 'a code
end
=
struct
  type _ t =
    | Return : 'a code -> 'a t
    (* | Path : (string -> unit) code -> string t *)
    | Bind : 'a t * ('a code -> 'b t) -> 'b t
    | Backup : 'a t -> 'a t

  let const x = Return .< x >.

  let rec code : type s. s t -> s code = function
    | Return c -> c
    (* | Path pc -> *)
    (*   let id = digest pc in *)
    (*   .< let () = .~pc id in id >. *)
    | Bind (w, f) ->
      code (f (code w))
    | Backup w -> code w

  let rec eval_code : type s. s t -> s code = function
    | Return c -> c
    | Bind (w, f) -> build_code (f (eval_code w))
    | Backup w ->
      let id = digest (code w) in
      .< In_channel.with_file id ~f:(Marshal.from_channel) >.

  let submit c =
    Lwt_preemptive.detach Runcode.runcode c

  let rec build : type s. s t -> unit Lwt.t = function
    | Return _ -> Lwt.return ()
    | Bind (w, f) ->
      Lwt.join [ build w ; build (f (eval_code w)) ]
    | Backup w ->
      build w >>= fun () ->
      submit (build_code w)

  let rec eval : type s. s t -> s Lwt.t = fun w ->
    build w >>=
    | Return (c, `not_cached) -> submit c
    | Return (

  let rec kind : type s. s t -> [`cached | `not_cached | `path] =
    | Return (_, `cached _) -> `cached
    | Return (_, `not_cached) -> `not_cached
    | Path _ -> `path
    | Bind (w, f) -> kind (f (code w))


  let rec build_code : type s. s t -> unit code = function
    | Return (c, `cached bc) ->
        let id = digest bc in .< .~bc id >.

    | Return (c, `not_cached) -> .< () >.

    | Path pc ->
      let id = digest pc in
      .< .~pc id >.

    | Bind (w, f) ->
      build_code (f (eval_code w))

  and eval_code : type s. s t -> s code = function w
    | Return (_, `cached bc) ->
      let id = digest bc in
      .< In_channel.with_file id ~f:(Marshal.from_channel) >.

    | Return (c, `not_cached) -> c

    | Path pc ->
      let id = digest pc in
      .< id >.

    | Bind (w, f) ->
      eval_code (f (eval_code w))

end

open Workflow

let addition x y =
  backup (
    x >>= fun x_ ->
    y >>= fun y_ ->
    return .< .~x_ + .~y_ >.
  )

let int_to_file i =
  i >>= fun i_ ->
  returnp .<
    fun output ->
      let oc = open_out output in
      Printf.fprintf oc "%d\n" .~i_ ;
      Out_channel.close oc
    >.

let pipeline =
  addition (const 1) (const 1)
(*  |> int_to_file *)

let () =
  Print_code.print_code Format.std_formatter (Workflow.build_code pipeline) ;
  Format.print_newline () ;
  Format.print_newline () ;
  Print_code.print_code Format.std_formatter (Workflow.eval_code pipeline) ;
  Format.print_newline () ;
  Format.print_newline () ;
  Print_code.print_code Format.std_formatter (Workflow.code pipeline)
