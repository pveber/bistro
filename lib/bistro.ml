open Core.Std

type path = string list
with sexp

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let ( % ) f g x = g (f x)

let ok = function
  | `Ok x -> x
  | `Error e -> raise e

let ( >>=& ) x ~f =
  match x with
  | `Ok y -> `Ok y
  | `Error e -> `Error (f e)

module Utils = struct
  let python_version fmt =
    let regexp = match fmt with
      | `M_m -> "[0-9]\\.[0-9]"
    in
    let ic = Unix.open_process_in (sprintf "python --version 2>&1 | grep -o '%s'" regexp) in
    let r = In_channel.input_line ic in
    In_channel.close ic ;
    r
end

type u =
  | Input of string * path
  | Extract of string * u * path
  | Step of step

and step = {
  id : string ;
  descr : string ;
  deps : u list ;
  script : script ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
}

and script = {
  interpreter : interpreter ;
  tokens : token list ;
}

and token =
  | S of string
  | D of u
  | DEST
  | TMP

and interpreter = [
  | `bash
  | `ocaml
  | `ocamlscript
  | `perl
  | `python
  | `R
  | `sh
]

with sexp

type 'a workflow = u
type some_workflow = Workflow : _ workflow -> some_workflow
type 'a directory = [`directory of 'a]
type package = [`package] directory


module Script = struct
  type t = script

  let make interpreter xs = {
    interpreter ;
    tokens = List.concat xs
  }

  let deps s =
    List.filter_map s.tokens ~f:(function
        | D r -> Some (r :> u)
        | S _ | DEST | TMP -> None
      )
    |> List.dedup

  let string_of_token ~string_of_workflow ~tmp ~dest = function
    | S s -> s
    | D w -> string_of_workflow (w :> u)
    | DEST -> dest
    | TMP -> tmp

  let to_string ~string_of_workflow ~tmp ~dest script =
    List.map script.tokens ~f:(string_of_token ~string_of_workflow ~tmp ~dest)
    |> String.concat
end


module Workflow = struct
  type 'a t = u

  let id = function
    | Input (id, _)
    | Extract (id, _, _)
    | Step { id } -> id

  let input ?(may_change = false) target =
    if not (Sys.file_exists_exn ~follow_symlinks:true target)
    then invalid_argf "Bistro.Worflow.input: path %s does not exist" target () ;
    let hash = if may_change then Some (Digest.file target) else None in
    let id = digest ("input", target, hash) in
    Input (id, path_of_string target)


  let make
      ?(descr = "")
      ?(mem = 100)
      ?(np = 1)
      ?(timeout = 24)
      ?version
      script =
    let deps = Script.deps script in
    let id = digest ("step",
                     version,
                     Script.to_string
                       ~string_of_workflow:id
                       ~tmp:"TMP"
                       ~dest:"DEST"
                       script) in
    Step { descr ; deps ; script ; np ; mem ; timeout ; version ; id }

  let extract u path =
    let u, path =
      match u with
      | Extract (_, v, p) -> v, p @ path
      | Input _ | Step _ -> u, path
    in
    let id = digest ("extract", id u, path) in
    Extract (id, u, path)
end

module EDSL = struct
  type expr = token list

  let workflow ?descr ?mem ?np ?timeout ?version ?(interpreter = `sh) expr =
    Workflow.make ?descr ?mem ?timeout ?version (Script.make interpreter expr)

  let dest = [ DEST ]
  let tmp = [ TMP ]
  let string s = [ S s ]
  let int i = [ S (string_of_int i) ]
  let float f = [ S (Float.to_string f) ]
  let path p = [ S (string_of_path p) ]
  let dep w = [ D w ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:[ S sep ]
    |> List.concat

  let seq ?(sep = "") xs = List.concat (List.intersperse ~sep:(string sep) xs)

  let enum dic x = [ S (List.Assoc.find_exn dic x) ]

  let use s = s.tokens
end

module EDSL_sh = struct
  include EDSL

  type cmd = token list

  let script cmds =
    Script.make
      `sh
      (List.intersperse ~sep:[S "\n"] cmds)

  let workflow ?descr ?mem ?np ?timeout ?version cmds =
    Workflow.make ?descr ?mem ?timeout ?version (script cmds)


  let cmd ?path ?pythonpath p ?stdin ?stdout ?stderr args =
    let add_path =
      match path with
      | None | Some [] -> ident
      | Some pkgs ->
        fun cmd ->
          S "(export PATH="
          :: (
            List.map pkgs ~f:(fun p -> [ D p ; S "/bin" ])
            |> List.intersperse ~sep:[S ":"]
            |> List.concat
          )
          @ [ S ":$PATH ; " ]
          @ cmd
          @ [ S ")" ]
    in
    let add_pythonpath = match pythonpath with
      | None | Some [] -> ident
      | Some pkgs ->
        fun cmd ->
          S "(export PYTHONPATH="
          :: (
            List.map pkgs ~f:(fun p -> [ D p ; S "/lib/python2.7/site-packages" ])
            (* FIXME: this won't work with other versions of python
               than 2.7 ; we should introduce execution-time variables
               -- here PYTHON_VERSION -- and the corresponding
               constructor in the API *)
            |> List.intersperse ~sep:[S ":"]
            |> List.concat
          )
          @ [ S ":$PYTHONPATH ; " ]
          @ cmd
          @ [ S ")" ]
    in
    let prog_expr = [ S p ] in
    let stdout_expr =
      match stdout with
      | None -> []
      | Some e -> S " > " :: e
    in
    let stdin_expr =
      match stdin with
      | None -> []
      | Some e -> S " < " :: e
    in
    let stderr_expr =
      match stderr with
      | None -> []
      | Some e -> S " 2> " :: e
    in
    [ prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
    |> List.filter ~f:(( <> ) [])
    |> List.intersperse ~sep:[S " "]
    |> List.concat
    |> add_pythonpath
    |> add_path


  let opt o f x = S o :: S " " :: f x

  let opt' o f x = S o :: S "=" :: f x

  let flag f x b = if b then f x else []

  let mkdir d = cmd "mkdir" [ d ]

  let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

  let cd p = cmd "cd" [ p ]

  let rm_rf x = cmd "rm" [ string "-rf" ; x ]

  let mv x y = cmd "mv" [ x ; y ]

  let wget url ?dest () = cmd "wget" [
      option (opt "-O" ident) dest ;
      string url
    ]

  let ( // ) x y = x @ [ S "/" ; S y ]

  let par cmd =
    S "( " :: (cmd @ [ S " )" ])

  let cmd_list op cmds =
    List.intersperse ~sep:[ S " " ; S op ; S " " ] cmds
    |> List.concat
    |> par

  let or_list = cmd_list "||"
  let and_list = cmd_list "&&"
  let pipe = cmd_list "|"

  let with_env vars cmd =
    (
      List.map vars ~f:(fun (var, value) -> [ S var ; S "=" ] @ value)
      |> List.intersperse ~sep:[ S " " ]
      |> List.concat
    )
    @ (S " " :: cmd)
end


module Db = struct

  (* this type should stay marshalable, because it is passed around in
     closures to workers. Not sure this can't be fixed, BTW*)
  type t = string

  let cache_dir db = Filename.concat db "cache"
  let build_dir db = Filename.concat db "build"
  let tmp_dir db = Filename.concat db "tmp"
  let stderr_dir db = Filename.concat db "stderr"
  let stdout_dir db = Filename.concat db "stdout"
  let stats_path db = Filename.concat db "stats"

  exception Corrupted_dbm

  let with_dbm db f =
    match Dbm.(opendbm (stats_path db) [ Dbm_create ; Dbm_rdwr ] 0o700) with
    | dbh ->
      let r = match f dbh with
        | x -> `Ok x
        | exception exn -> `Error (`Exn exn)
      in
      Dbm.close dbh ;
      r
    | exception exn -> `Error `Corrupted_dbm

  let with_dbm_exn db f =
    match with_dbm db f with
    | `Ok () -> ()
    | `Error `Corrupted_dbm ->
      failwithf "Corrupted db at %s (corrupted dbm records)" db ()
    | `Error (`Exn exn) -> raise exn

  let assert_no_exn = function
    | `Exn _ -> assert false
    | `Malformed_db s -> `Malformed_db s
    | `Corrupted_dbm -> `Corrupted_dbm

  let well_formed_db path =
    let open Pvem.Result in
    let check path =
      if Sys.file_exists_exn path then `Ok ()
      else `Error (`Malformed_db (path ^" doesn't exist"))
    in
    check path >>= fun () ->
    check (cache_dir path) >>= fun () ->
    check (build_dir path) >>= fun () ->
    check (tmp_dir path) >>= fun () ->
    check (stderr_dir path) >>= fun () ->
    check (stdout_dir path) >>= fun () ->
    check ((stats_path path) ^ ".pag") >>= fun () ->
    check ((stats_path path) ^ ".dir") >>= fun () ->
    with_dbm path (const ()) >>=&
    assert_no_exn

  let init base =
    let open Pvem.Result in
    if Sys.file_exists_exn base
    then (
      well_formed_db base >>= fun () ->
      return base
    )
    else (
      Unix.mkdir_p (tmp_dir base) ;
      Unix.mkdir_p (build_dir base) ;
      Unix.mkdir_p (cache_dir base) ;
      Unix.mkdir_p (stderr_dir base) ;
      Unix.mkdir_p (stdout_dir base) ;
      with_dbm base ignore >>=&
      assert_no_exn >>= fun () ->
      return base
    )

  let init_exn base =
    match init base with
    | `Ok db -> db
    | `Error e ->
      let explanation = match e with
        | `Corrupted_dbm -> "corrupted dbm"
        | `Malformed_db s -> s
      in
      failwithf
        "Bistro_db.init_exn: the path %s is not available for a bistro database or has been corrupted (%s)" base explanation ()

  let aux_path f db step =
    Filename.concat (f db) step.id

  let cache_path = aux_path cache_dir
  let build_path = aux_path build_dir
  let tmp_path = aux_path tmp_dir
  let stdout_path = aux_path stdout_dir
  let stderr_path = aux_path stderr_dir

  module Stats = struct
    type t = {
      workflow : step ;
      history : (Time.t * event) list ;
      build_time : float option ;
    }
    and event = Built | Requested
    with sexp

    let make s = {
      workflow = s ;
      history = [] ;
      build_time = None ;
    }

    let load db step =
      match Dbm.find db step.id with
      | sexp -> Some (t_of_sexp (Sexp.of_string sexp))
      | exception Not_found -> None

    let save db step stats =
      Dbm.replace db step.id (Sexp.to_string (sexp_of_t stats))
  end

  let update_stats db step f =
    with_dbm_exn db (fun dbh ->
        let stat =
          match Stats.load dbh step with
          | Some s -> s
          | None -> Stats.make step
        in
        Stats.save dbh step (f stat)
      )
  let append_history ~db u evt =
    update_stats db u (fun stat ->
        { stat with
          Stats.history = (Time.now (), evt) :: stat.Stats.history }
      )

  let requested db step =
    append_history ~db step Stats.Requested

  let built db step =
    append_history ~db step Stats.Built

  let rec workflow_path db = function
    | Extract (_, dir, p) ->
      List.fold (workflow_path db dir :: p) ~init:"" ~f:Filename.concat
    | Input (_, p) -> string_of_path p
    | Step step -> cache_path db step


  (* let log db fmt = *)
  (*   let f msg = *)
  (*     let path = *)
  (*       Filename.concat *)
  (*         (log_dir db) *)
  (*         (Time.format (Time.now ()) "%Y-%m-%d.log") *)
  (*     in *)
  (*     echo ~path msg *)
  (*   in *)
  (*   Printf.ksprintf f fmt *)
end

module type Configuration = sig
  val db_path : string
  val np : int
  val mem : int
end


(* module Utils = struct *)
(*   let load_value fn = *)
(*     In_channel.with_file fn ~f:(Marshal.from_channel) *)

(*   let save_value fn v = *)
(*     Out_channel.with_file fn ~f:(fun oc -> Marshal.to_channel oc v []) *)

(*   let with_temp_file ~in_dir ~f = *)
(*     let fn = Filename.temp_file ~in_dir "bistro" "" in *)
(*     protect ~f:(fun () -> f fn) ~finally:(fun () -> Sys.remove fn) *)

(*   type 'a logger = ('a,unit,string,unit) format4 -> 'a *)

(*   let null_logger fmt = *)
(*     Printf.ksprintf ignore fmt *)

(*   let printer oc fmt = *)
(*     let open Unix in *)
(*     let open Printf in *)
(*     ksprintf ident fmt *)

(*   let logger (type s) oc label fmt : s logger = *)
(*     let open Unix in *)
(*     let open Printf in *)
(*     let f msg = *)
(*       let t = localtime (time ()) in *)
(*       fprintf *)
(*         oc "[%s][%04d-%02d-%02d %02d:%02d] %s\n%!" *)
(*         label (1900 + t.tm_year) (t.tm_mon + 1)t.tm_mday t.tm_hour t.tm_min msg *)
(*     in *)
(*     ksprintf f fmt *)

(*   let sh ?(stdout = stdout) ?(stderr = stderr) cmd = *)
(*     try *)
(*       Shell.call *)
(*         ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout)) *)
(*         ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr)) *)
(*         [ Shell.cmd "sh" [ "-c" ; cmd ] ] *)
(*     with Shell.Subprocess_error _ -> ( *)
(*         Core.Std.failwithf "shell call failed:\n%s\n" cmd () *)
(*       ) *)

(*   let shf ?(stdout = stdout) ?(stderr = stderr) fmt = *)
(*     Printf.ksprintf (sh ~stdout ~stderr) fmt *)
(* end *)

module Pool : sig
  type t

  val create : np:int -> mem:int -> t
  val use : t -> np:int -> mem:int -> f:(np:int -> mem:int -> 'a Lwt.t) -> 'a Lwt.t
end =
struct
  let ( >>= ) = Lwt.( >>= )

  type t = {
    np : int ;
    mem : int ;
    mutable current_np : int ;
    mutable current_mem : int ;
    mutable waiters : ((int * int) * unit Lwt.u) list ;
  }

  let create ~np ~mem = {
    np ; mem ;
    current_np = np ;
    current_mem = mem ;
    waiters = [] ;
  }

  let decr p ~np ~mem =
    p.current_np <- p.current_np - np ;
    p.current_mem <- p.current_mem - mem

  let incr p ~np ~mem =
    p.current_np <- p.current_np + np ;
    p.current_mem <- p.current_mem + mem

  let acquire p ~np ~mem =
    if np <= p.current_np && mem <= p.current_mem then (
      decr p ~np ~mem ;
      Lwt.return ()
    )
    else (
      let t, u = Lwt.wait () in
      p.waiters <- ((np,mem), u) :: p.waiters ;
      t
    )

  let release p ~np ~mem =
    let rec wake_guys_up p = function
      | [] -> []
      | (((np, mem), u) as h) :: t ->
        if np <= p.current_np && mem <= p.current_mem then (
          decr p ~np ~mem ;
          Lwt.wakeup u () ;
          t
        )
        else h :: (wake_guys_up p t)
    in
    incr p ~np ~mem ;
    p.waiters <- wake_guys_up p (List.sort (fun (x, _) (y,_) -> compare y x) p.waiters)

  let use p ~np ~mem ~f =
    if np > p.np then
      Lwt.fail (Invalid_argument "Bistro.Pool: asked more processors than there are in the pool")
    else if mem > p.mem then
      Lwt.fail (Invalid_argument "Bistro.Pool: asked more memory than there is in the pool")
    else (
      acquire p ~np ~mem >>= fun () ->
      Lwt.catch
        (fun () ->
           f ~np ~mem >>= fun r -> Lwt.return (`result r))
        (fun exn -> Lwt.return (`error exn))
      >>= fun r ->
      release p ~np ~mem ;
      match r with
      | `result r -> Lwt.return r
      | `error exn -> Lwt.fail exn
    )
end

type build_result = [`Ok of unit | `Error of (u * string) list]

(* Currently Building Steps

     If two threads try to concurrently execute a step, we don't want
     the build procedure to be executed twice. So when the first
     thread tries to eval the workflow, we store the build thread in a
     hash table. When the second thread tries to eval, we give the
     build thread in the hash table, which prevents the workflow from
     being built twice concurrently.

*)
module CBST :
sig
  type t
  val create : unit -> t
  val find_or_add : t -> step -> (unit -> build_result Lwt.t) -> build_result Lwt.t
  val join : t -> unit Lwt.t
end
=
struct
  module S = struct
    type t = step
    let equal x y = x.id = y.id
    let hash x = String.hash x.id
  end

  module T = Caml.Hashtbl.Make(S)

  type contents =
    | Thread of build_result Lwt.t

  type t = contents T.t

  let create () = T.create 253


  let find_or_add table x f =
    let open Lwt in
    match T.find table x with
    | Thread t -> t
    | exception Not_found ->
      let waiter, u = Lwt.wait () in
      T.add table x (Thread waiter) ;
      Lwt.async (fun () ->
          f () >>= fun res ->
          T.remove table x ;
          Lwt.wakeup u res ;
          Lwt.return ()
        ) ;
      waiter

  let join table =
    let f _ (Thread t) accu = (Lwt.map ignore t) :: accu in
    T.fold f table []
    |> Lwt.join
end

module Engine = struct

  open Lwt
  let ( >>=? ) x f = x >>= function
    | `Ok x -> f x
    | `Error _ as e -> return e

  type t = {
    db : Db.t ;
    pool : Pool.t ;
    cbs : CBST.t ;
    mutable on : bool ;
  }

  let make ~np ~mem db = {
    db ;
    pool = Pool.create ~np ~mem ;
    cbs = CBST.create () ;
    on = true ;
  }

  let remove_if_exists fn =
    if Sys.file_exists fn = `Yes then
      Lwt_process.exec ("", [| "rm" ; "-r" ; fn |]) >|= ignore
    else
      Lwt.return ()

  let redirection filename =
    Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
    Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

  let submit_script e ~np ~mem ~timeout ~stdout ~stderr ~interpreter script =
    Pool.use e.pool ~np ~mem ~f:(fun ~np ~mem ->
        match interpreter with
        | `sh ->
          let script_file = Filename.temp_file "guizmin" ".sh" in
          Lwt_io.(with_file ~mode:output script_file (fun oc -> write oc script)) >>= fun () ->
          redirection stdout >>= fun stdout ->
          redirection stderr >>= fun stderr ->
          let cmd = ("", [| "sh" ; script_file |]) in
          Lwt_process.exec ~stdout ~stderr cmd >>=
          begin
            function
            | Caml.Unix.WEXITED 0 ->
              Lwt_unix.unlink script_file >>= fun () ->
              Lwt.return `Ok
            | _ ->
              Lwt.return (`Error `Script_failure)
          end
        | _ -> Lwt.return (`Error `Unsupported_interpreter)
      )

  let join_results xs =
    let f accu x =
      x >>= function
      | `Ok () -> return accu
      | `Error errors as e ->
        match accu with
        | `Ok _ -> return e
        | `Error errors' -> return (`Error (errors @ errors'))
    in
    Lwt_list.fold_left_s f (`Ok ()) xs


  let rec build_workflow e = function
    | Input _ as i -> build_input e i
    | Extract (_,dir,p) as x -> build_extract e x dir p
    | Step step as w ->
      Db.requested e.db step ;
      let dest = Db.workflow_path e.db w in
      if Sys.file_exists dest = `Yes then
        Lwt.return (`Ok ())
      else
        CBST.find_or_add e.cbs step (fun () ->
            let dep_threads = List.map step.deps ~f:(build_workflow e) in
            build_step e step dep_threads
          )

  and build_step
      e
      ({ np ; mem ; timeout ; script } as step)
      dep_threads =

    join_results dep_threads >>=? fun () ->
    (
      let stdout = Db.stdout_path e.db step in
      let stderr = Db.stderr_path e.db step in
      let dest = Db.build_path e.db step in
      let tmp = Db.tmp_path e.db step in
      let script_text =
        Script.to_string ~string_of_workflow:(Db.workflow_path e.db) ~dest ~tmp script
      in
      remove_if_exists stdout >>= fun () ->
      remove_if_exists stderr >>= fun () ->
      remove_if_exists dest >>= fun () ->
      remove_if_exists tmp >>= fun () ->
      Lwt_unix.mkdir tmp 0o750 >>= fun () ->
      submit_script
        e ~np ~mem ~timeout ~stdout ~stderr
        ~interpreter:script.interpreter script_text >>= fun response ->
      match response, Sys.file_exists_exn dest with
      | `Ok, true ->
        remove_if_exists tmp >>= fun () ->
        Db.built e.db step ;
        Lwt_unix.rename dest (Db.cache_path e.db step) >>= fun () ->
        Lwt.return (`Ok ())
      | `Ok, false ->
        let msg =
          "Workflow failed to produce its output at the prescribed location."
        in
        Lwt.return (`Error [ Step step, msg ])
      | `Error `Script_failure, _ ->
        let msg = "Script failed" in
        return (`Error [ Step step, msg ])
      | `Error `Unsupported_interpreter, _ ->
        let msg =
          "Unsupported interpreter"
        in
        return (`Error [ Step step, msg])
  )

  and build_input e i =
    Lwt.wrap (fun () ->
        let p = Db.workflow_path e.db i in
        if Sys.file_exists p <> `Yes then
          let msg =
            sprintf
              "File %s is declared as an input of a workflow but does not exist."
              p
          in
          `Error [ i, msg ]
        else
          `Ok ()
      )

  and build_extract e x dir p =
    let p = string_of_path p in
    let dir_path = Db.workflow_path e.db dir in
    let check_in_dir () =
      if Sys.file_exists (Db.workflow_path e.db x) <> `Yes
      then (
        let msg =
          sprintf "No file or directory named %s in directory workflow %s."
            p
            dir_path
        in
        return (`Error [ x, msg ])
      )
      else return (`Ok ())
    in
    if Sys.file_exists dir_path = `Yes then (
      check_in_dir () >>=? fun () ->
      let () = match dir with
        | Input _ -> ()
        | Extract _ -> assert false
        | Step s -> Db.requested e.db s
      in
      return (`Ok ())
    )
    else (
      let dir_thread = build_workflow e dir in
      dir_thread >>=? check_in_dir
    )


  let build e w =
    (
      if e.on then
        build_workflow e w
      else
        Lwt.return (`Error [w, "Engine_halted"])
    )
    >>= function
    | `Ok () -> Lwt.return (`Ok (Db.workflow_path e.db w))
    | `Error xs ->
      Lwt.return (`Error (List.map xs ~f:(fun (w, s) -> Workflow w, s)))

  let build_exn e w =
    build e w >>= function
    | `Ok s -> Lwt.return s
    | `Error xs ->
      let msgs = List.map ~f:(fun (Workflow w, msg) -> Workflow.id w ^ "\t" ^ msg) xs in
      let msg = sprintf "Some build(s) failed:\n\t%s\n" (String.concat ~sep:"\n\t" msgs) in
      Lwt.fail (Failure msg)

  let shutdown e =
    e.on <- false ;
    CBST.join e.cbs

end
