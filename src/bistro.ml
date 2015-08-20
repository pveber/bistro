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
  interpreter : interpreter ;
  descr : string ;
  deps : u list ;
  script : script ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
}

and script = token list

and token =
  | S of string
  | D of u
  | DEST
  | TMP

and interpreter = [
  | `bash
  | `ocaml of string list
  | `perl
  | `python
  | `R
  | `sh
]

with sexp

module Types = struct
  type 'a workflow = u

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type package = [`package] directory

  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file
  type bash_script = ([`bash_script], [`text]) file

  class type ['a] tabular = object ('a)
    constraint 'a = < columns : 'b ; header : ([< `yes | `no] as 'c) ;
                      sep : 'd ; comment : 'e ; .. >
    inherit [[`tabular], [`text]] file
    method columns : 'b
    method header : 'c
    method sep : 'd
    method comment : 'e
  end

  class type ['a] tsv = object
    inherit [ < sep : [`tab] ; .. > as 'a ] tabular
  end
end

module Script = struct
  type t = script

  let deps l =
    List.filter_map l ~f:(function
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
    List.map script ~f:(string_of_token ~string_of_workflow ~tmp ~dest)
    |> String.concat

  module Shell = struct
    type expr = token list
    type cmd = token list

    let script cmds =
      List.intersperse ~sep:[S "\n"] cmds
      |> List.concat

    let program ?path ?pythonpath p ?stdin ?stdout ?stderr args =
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

    let opt o f x = S o :: S " " :: f x

    let opt' o f x = S o :: S "=" :: f x

    let flag f x b = if b then f x else []

    let mkdir d = program "mkdir" [ d ]

    let mkdir_p d = program "mkdir" [ string "-p" ; d ]

    let cd p = program "cd" [ p ]

    let rm_rf x = program "rm" [ string "-rf" ; x ]

    let mv x y = program "mv" [ x ; y ]

    let wget url ?dest () = program "wget" [
        option (opt "-O" ident) dest ;
        string url
      ]

    let bash ?path script ?stdin ?stdout ?stderr args =
      program "bash" ?path ?stdin ?stdout ?stderr (dep script :: args)

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
      ?(interpreter = `bash)
      ?(mem = 100)
      ?(np = 1)
      ?(timeout = 24)
      ?version
      script =
    let deps = Script.deps script in
    let id = digest ("step",
                     interpreter,
                     version,
                     Script.to_string
                       ~string_of_workflow:id
                       ~tmp:"TMP"
                       ~dest:"DEST"
                       script) in
    Step { descr ; interpreter ; deps ; script ; np ; mem ; timeout ; version ; id }

  let extract u path =
    let u, path =
      match u with
      | Extract (_, v, p) -> v, p @ path
      | Input _ | Step _ -> u, path
    in
    let id = digest ("extract", id u, path) in
    Extract (id, u, path)
end


module Std = struct
  include Types
  module Script = Script
  module Workflow = Workflow
end

type 'a workflow = 'a Std.workflow

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

  let rec requested : type s. t -> s workflow -> unit = fun db -> function
    | Extract (_, u, _) -> requested db u
    | Input _ -> ()
    | Step step -> append_history ~db step Stats.Requested

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

(* module Pool : sig *)
(*   type t *)

(*   val create : np:int -> mem:int -> t *)
(*   val use : t -> np:int -> mem:int -> f:(np:int -> mem:int -> 'a Lwt.t) -> 'a Lwt.t *)
(* end = *)
(* struct *)
(*   let ( >>= ) = Lwt.( >>= ) *)

(*   type t = { *)
(*     np : int ; *)
(*     mem : int ; *)
(*     mutable current_np : int ; *)
(*     mutable current_mem : int ; *)
(*     mutable waiters : ((int * int) * unit Lwt.u) list ; *)
(*   } *)

(*   let create ~np ~mem = { *)
(*     np ; mem ; *)
(*     current_np = np ; *)
(*     current_mem = mem ; *)
(*     waiters = [] ; *)
(*   } *)

(*   let decr p ~np ~mem = *)
(*     p.current_np <- p.current_np - np ; *)
(*     p.current_mem <- p.current_mem - mem *)

(*   let incr p ~np ~mem = *)
(*     p.current_np <- p.current_np + np ; *)
(*     p.current_mem <- p.current_mem + mem *)

(*   let acquire p ~np ~mem = *)
(*     if np <= p.current_np && mem <= p.current_mem then ( *)
(*       decr p ~np ~mem ; *)
(*       Lwt.return () *)
(*     ) *)
(*     else ( *)
(*       let t, u = Lwt.wait () in *)
(*       p.waiters <- ((np,mem), u) :: p.waiters ; *)
(*       t *)
(*     ) *)

(*   let release p ~np ~mem = *)
(*     let rec wake_guys_up p = function *)
(*       | [] -> [] *)
(*       | (((np, mem), u) as h) :: t -> *)
(*         if np <= p.current_np && mem <= p.current_mem then ( *)
(*           decr p ~np ~mem ; *)
(*           Lwt.wakeup u () ; *)
(*           t *)
(*         ) *)
(*         else h :: (wake_guys_up p t) *)
(*     in *)
(*     incr p ~np ~mem ; *)
(*     p.waiters <- wake_guys_up p (List.sort (fun (x, _) (y,_) -> compare y x) p.waiters) *)

(*   let use p ~np ~mem ~f = *)
(*     if np > p.np then Lwt.fail (Invalid_argument "Bistro.Pool: asked more processors than there are in the pool") *)
(*     else if mem > p.mem then Lwt.fail (Invalid_argument "Bistro.Pool: asked more memory than there is in the pool") *)
(*     else ( *)
(*       acquire p ~np ~mem >>= fun () -> *)
(*       Lwt.catch *)
(*         (fun () -> *)
(*            f ~np ~mem >>= fun r -> Lwt.return (`result r)) *)
(*         (fun exn -> Lwt.return (`error exn)) *)
(*       >>= fun r -> *)
(*       release p ~np ~mem ; *)
(*       match r with *)
(*       | `result r -> Lwt.return r *)
(*       | `error exn -> Lwt.fail exn *)
(*     ) *)
(* end *)


(* type 'a path = Path of string *)

(* type env = { *)
(*   sh : string -> unit ; (\** Execute a shell command (with {v /bin/sh v}) *\) *)
(*   shf : 'a. ('a,unit,string,unit) format4 -> 'a ; *)
(*   stdout : out_channel ; *)
(*   stderr : out_channel ; *)
(*   out : 'a. ('a,out_channel,unit) format -> 'a ; *)
(*   err : 'a. ('a,out_channel,unit) format -> 'a ; *)
(*   with_temp_file : 'a. (string -> 'a) -> 'a ; *)
(* } *)


(* type primitive_info = { *)
(*   id : string ; *)
(*   version : int option ; *)
(* } with sexp *)

(* type _ workflow = *)
(*   | Input : string * string -> 'a path workflow *)
(*   | Value_workflow : string * (env -> 'a) term -> 'a workflow *)
(*   | Path_workflow : string * (string -> env -> unit) term -> 'a path workflow *)
(*   | Extract : string * [`directory of 'a] path workflow * string list -> 'b path workflow *)

(* and _ term = *)
(*   | Prim : primitive_info * 'a -> 'a term *)
(*   | App : ('a -> 'b) term * 'a term * string option -> 'b term *)
(*   | String : string -> string term *)
(*   | Int : int -> int term *)
(*   | Bool : bool -> bool term *)
(*   | Workflow : 'a workflow -> 'a term *)
(*   | Option : 'a term option -> 'a option term *)
(*   | List : 'a term list -> 'a list term *)

(* let primitive_info id ?version () = { *)
(*   id ; version ; *)
(* } *)

(* module Term = struct *)
(*   type 'a t = 'a term *)

(*   let prim id ?version x = *)
(*     Prim (primitive_info id ?version (), x) *)

(*   let app ?n f x = App (f, x, n) *)

(*   let ( $ ) f x = app f x *)

(*   let string s = String s *)
(*   let int i = Int i *)
(*   let bool b = Bool b *)
(*   let option f x = Option (Option.map x ~f) *)
(*   let list f xs = List (List.map xs ~f) *)
(*   let workflow w = Workflow w *)
(* end *)


(* let digest x = *)
(*   Digest.to_hex (Digest.string (Marshal.to_string x [])) *)

(* module Description = struct *)
(*   type workflow = *)
(*     | Input of string *)
(*     | Value_workflow of term *)
(*     | Path_workflow of term *)
(*     | Extract of workflow * string list *)
(*   and term = *)
(*     | Prim of primitive_info *)
(*     | App of term * term * string option *)
(*     | String of string *)
(*     | Int of int *)
(*     | Bool of bool *)
(*     | Workflow of workflow *)
(*     | Option of term option *)
(*     | List of term list *)
(*   with sexp *)
(* end *)


(* let rec term_description : type s. s term -> Description.term = function *)
(*   | Prim (info, _) -> Description.Prim info *)

(*   | App (f, x, lab) -> *)
(*     Description.App (term_description f, *)
(*                      term_description x, *)
(*                      lab) *)

(*   | String s -> Description.String s *)
(*   | Int i -> Description.Int i *)
(*   | Bool b -> Description.Bool b *)
(*   | Workflow w -> Description.Workflow (workflow_description w) *)
(*   | Option o -> Description.Option (Option.map o ~f:term_description) *)
(*   | List l -> Description.List (List.map l ~f:term_description) *)

(* and workflow_description : type s. s workflow -> Description.workflow = function *)
(*   | Input (_, fn) -> Description.Input fn *)
(*   | Value_workflow (_, t) -> Description.Value_workflow (term_description t) *)
(*   | Path_workflow (_, t) -> Description.Path_workflow (term_description t) *)
(*   | Extract (_, dir, path) -> Description.Extract (workflow_description dir, path) *)

(* let sexp_of_workflow w = *)
(*   Description.sexp_of_workflow (workflow_description w) *)

(* let workflow t = Value_workflow (digest (`workflow (term_description t)), t) *)
(* let path_workflow t = Path_workflow (digest (`path_workflow (term_description t)), t) *)

(* let rec extract : type s. [`directory of s] path workflow -> string list -> 'a workflow = fun dir path -> *)
(*   match dir with *)
(*   | Extract (_, dir', path') -> extract dir' (path' @ path) *)
(*   | Path_workflow _ -> extract_aux dir path *)
(*   | Input (_, fn) -> extract_aux dir path *)
(*   | Value_workflow _ -> assert false (\* unreachable case, due to typing constraints *\) *)
(* and extract_aux : type s. [`directory of s] path workflow -> string list -> 'a workflow = fun dir path -> *)
(*   let id = digest (`extract (workflow_description dir, path)) in *)
(*   Extract (id, dir , path) *)

(* let input fn = Input (digest (`input fn), fn) *)

(* let id : type s. s workflow -> string = function *)
(*   | Input (id, fn) -> id *)
(*   | Value_workflow (id, _) -> id *)
(*   | Path_workflow (id, _) -> id *)
(*   | Extract (id, _, _) -> id *)







(* (\* type 'a iterator = { f : 'b. 'a -> 'b workflow -> 'a } *\) *)

(* (\* let rec fold_deps_in_term : type s. s term -> init:'a -> it:'a iterator -> 'a = fun t ~init ~it -> *\) *)
(* (\*   match t with *\) *)
(* (\*   | String _ -> init *\) *)
(* (\*   | Int _ -> init *\) *)
(* (\*   | Bool _ -> init *\) *)
(* (\*   | Option None -> init *\) *)
(* (\*   | Prim _ -> init *\) *)
(* (\*   | App (f, x, _) -> *\) *)
(* (\*     let init = fold_deps_in_term f ~init ~it in *\) *)
(* (\*     fold_deps_in_term x ~init ~it *\) *)
(* (\*   | Value_workflow w -> it.f init w *\) *)
(* (\*   | Option (Some t) -> *\) *)
(* (\*     fold_deps_in_term t ~init ~it *\) *)
(* (\*   | List ts -> *\) *)
(* (\*     List.fold ts ~init ~f:(fun accu t -> fold_deps_in_term t ~init:accu ~it) *\) *)

(* (\* let rec fold_deps : type s. s workflow -> init:'a -> it:'a iterator -> 'a = fun w ~init ~it -> *\) *)
(* (\*   match w with *\) *)
(* (\*   | Value_workflow t -> fold_deps_in_term t ~init ~it *\) *)
(* (\*   | File t -> fold_deps_in_term t ~init ~it *\) *)
(* (\*   | Directory t -> fold_deps_in_term t ~init ~it *\) *)
(* (\*   | Extract (dir, path) -> fold_deps dir ~init ~it *\) *)

(* let remove_if_exists fn = *)
(*   if Sys.file_exists_exn fn *)
(*   then Sys.command (sprintf "rm -r %s" fn) |> ignore *)


module Engine(Conf : Configuration) = struct
  open Lwt

  let db = Db.init_exn Conf.db_path

(*   let worker_pool = *)
(*     if !Sys.interactive then None *)
(*     else Some (fst (Nproc.create Conf.np)) *)

(*   let submit f = *)
(*     match worker_pool with *)
(*     | None -> *)
(*       Lwt_preemptive.detach f () >|= fun x -> Some x *)
(*     | Some pool -> *)
(*       Nproc.submit pool ~f () *)

(*   let np = match worker_pool with *)
(*     | None -> 1 *)
(*     | Some _ -> Conf.np *)

(*   let mem = Conf.mem *)

(*   let resource_pool = Pool.create ~np ~mem *)

(*   let with_env ~np ~mem x ~f = *)
(*     let stderr = open_out (sprintf "%s/%s" (Db.stderr_dir db) (id x)) in *)
(*     let stdout = open_out (sprintf "%s/%s" (Db.stdout_dir db) (id x)) in *)
(*     let env = { *)
(*       stderr ; stdout ; *)
(*       out = (fun fmt -> fprintf stdout fmt) ; *)
(*       err = (fun fmt -> fprintf stderr fmt) ; *)
(*       shf = (fun fmt -> Utils.shf ~stdout ~stderr fmt) ; *)
(*       sh = Utils.sh ~stdout ~stderr ; *)
(*       with_temp_file = fun f -> Utils.with_temp_file ~in_dir:(Db.tmp_dir db) ~f *)
(*     } *)
(*     in *)
(*     protect ~f:(fun () -> f env) ~finally:(fun () -> *)
(*         List.iter ~f:Out_channel.close [ stderr ; stdout ] *)
(*       ) *)

(*   let send_task w deps (t : env -> [`Ok | `Error of string]) = *)
(*     deps >>= fun () -> *)
(*     Pool.use resource_pool ~np:1 ~mem:100 ~f:(fun ~np ~mem -> *)
(*         let f () = with_env ~np ~mem w ~f:t in *)
(*         submit f >>= function *)
(*         | Some `Ok -> Lwt.return () *)
(*         | Some (`Error msg) -> Lwt.fail (Failure msg) *)
(*         | None -> Lwt.fail (Failure "A primitive raised an unknown exception") *)
(*       ) *)

(*   (\* Wrapping around a task, implementing: *)
(*      - removing previous stdout stderr tmp *)
(*      - moving from build to cache and clearing tmp *)
(*        if the task succeeded *)
(*   *\) *)
(*   let create_task x (f : env -> unit) = *)
(*     let stdout_path = Db.stdout_path db x in *)
(*     let stderr_path = Db.stderr_path db x in *)
(*     let tmp_path    = Db.tmp_path    db x in *)
(*     let build_path  = Db.build_path  db x in *)
(*     let cache_path  = Db.cache_path  db x in *)
(*     fun env -> *)
(*       remove_if_exists stdout_path ; *)
(*       remove_if_exists stderr_path ; *)
(*       remove_if_exists build_path  ; *)
(*       remove_if_exists tmp_path    ; *)
(*       Unix.mkdir tmp_path ; *)
(*       let outcome = try f env ; `Ok with exn -> `Error exn in *)
(*       match outcome, Sys.file_exists_exn build_path with *)
(*       | `Ok, true -> *)
(*         remove_if_exists tmp_path ; *)
(*         Unix.rename build_path cache_path ; *)
(*         `Ok *)
(*       | `Ok, false -> *)
(*         let msg = sprintf "Workflow %s failed to produce its target at the prescribed location" (id x) in *)
(*         `Error msg *)
(*       | `Error (Failure msg), _ -> *)
(*         let msg = sprintf "Workflow %s failed saying: %s" (id x) msg in *)
(*         `Error msg *)
(*       | `Error exn, _ -> raise exn *)

(*   (\* Currently Building Workflows *)

(*      If two threads try to concurrently eval a workflow, we don't want *)
(*      the build procedure to be executed twice. So when the first *)
(*      thread tries to eval the workflow, we store the build thread in a *)
(*      hash table. When the second thread tries to eval, we give the *)
(*      build thread in the hash table, which prevents the workflow from *)
(*      being built twice concurrently. *)

(*   *\) *)
(*   module CBW = struct *)
(*     module W = struct *)
(*       type t = W : _ workflow -> t *)
(*       let equal (W x) (W y) = id x = id y *)
(*       let hash (W x) = String.hash (id x) *)
(*     end *)

(*     module T = Caml.Hashtbl.Make(W) *)

(*     type contents = *)
(*       | Thread of unit Lwt.t *)

(*     let table : contents T.t = T.create 253 *)


(*     let find_or_add x f = *)
(*       match T.find table (W.W x) with *)
(*       | Thread t -> t *)
(*       | exception Not_found -> *)
(*         let waiter, u = Lwt.wait () in *)
(*         T.add table (W.W x) (Thread waiter) ; *)
(*         f () >>= fun () -> *)
(*         T.remove table (W.W x) ; *)
(*         Lwt.wakeup u () ; *)
(*         Lwt.return () *)
(*   end *)



(*   let rec build : type s. s workflow -> unit Lwt.t = fun w -> *)
(*     Db.requested db w ; *)
(*     CBW.find_or_add w (fun () -> *)
(*         match w with *)
(*         | Input (_, fn) -> build_input fn *)

(*         | Extract (_, dir, path_in_dir) -> *)
(*           build_extract (Db.cache_path db w) dir path_in_dir *)

(*         | Path_workflow (_, term_w) -> *)
(*           build_path_workflow w term_w *)

(*         | Value_workflow (_, term_w) -> *)
(*           build_workflow w term_w *)
(*       ) *)

(*   and build_input fn = *)
(*     let f () = *)
(*       if not (Sys.file_exists_exn fn) *)
(*       then failwithf "File %s is declared as an input of a workflow but does not exist." fn () *)
(*     in *)
(*     Lwt.wrap f *)

(*   and build_extract : type s. string -> s workflow -> string list -> unit Lwt.t = *)
(*     fun output dir path_in_dir -> *)
(*       let dir_path = Db.cache_path db dir in *)
(*       (\* Checks the file to extract of the directory is there *\) *)
(*       let check_in_dir () = *)
(*         if not (Sys.file_exists_exn output) *)
(*         then ( *)
(*           let msg = sprintf "No file or directory named %s in directory workflow." (String.concat ~sep:"/" path_in_dir) in *)
(*           Lwt.fail (Failure msg) *)
(*         ) *)
(*         else Lwt.return () *)
(*       in *)
(*       if Sys.file_exists_exn dir_path then check_in_dir () *)
(*       else build dir >>= check_in_dir *)

(*   and build_path_workflow : type s. s workflow -> (string -> env -> unit) Term.t -> unit Lwt.t = *)
(*     fun w term_w -> *)
(*       let cache_path = Db.cache_path db w in *)
(*       if Sys.file_exists_exn cache_path then Lwt.return () *)
(*       else *)
(*         let thunk, deps = compile_term term_w in *)
(*         let build_path = Db.build_path db w in *)
(*         let f env = (thunk ()) build_path env in *)
(*         send_task w deps (create_task w f) *)

(*   and build_workflow : type s. s workflow -> (env -> s) Term.t -> unit Lwt.t = *)
(*     fun w term_w -> *)
(*       let cache_path = Db.cache_path db w in *)
(*       if Sys.file_exists_exn cache_path then Lwt.return () *)
(*       else *)
(*         let thunk, deps = compile_term term_w in *)
(*         let build_path = Db.build_path db w in *)
(*         let f env = *)
(*           let y = (thunk ()) env in *)
(*           Utils.save_value build_path y *)
(*         in *)
(*         send_task w deps (create_task w f) *)

(*   and compile_term : type s. s term -> (unit -> s) * unit Lwt.t = function *)
(*     | Prim (_, v) -> const v, Lwt.return () *)
(*     | App (f, x, _) -> *)
(*       let ff, f_deps = compile_term f in *)
(*       let xx, x_deps = compile_term x in *)
(*       (fun () -> (ff ()) (xx ())), *)
(*       Lwt.join [ f_deps ; x_deps ] *)
(*     | Int i -> const i, Lwt.return () *)
(*     | String s -> const s, Lwt.return () *)
(*     | Bool b -> const b, Lwt.return () *)
(*     | Option None -> const None, Lwt.return () *)
(*     | Option (Some t) -> *)
(*       let tt,t_deps = compile_term t in *)
(*       (fun () -> Some (tt ())), t_deps *)
(*     | List ts -> *)
(*       let tts, deps = List.map ts ~f:compile_term |> List.unzip in *)
(*       (fun () -> List.map tts ~f:(fun ff -> ff ())), *)
(*       Lwt.join deps *)

(*     | Workflow (Value_workflow _ as w) -> *)
(*       (fun () -> Utils.load_value (Db.cache_path db w)), *)
(*       build w *)

(*     | Workflow (Path_workflow _ as w) -> *)
(*       (fun () -> Path (Db.cache_path db w)), *)
(*       build w *)

(*     | Workflow (Input (_, fn)) -> *)
(*       (fun () -> Path fn), *)
(*       Lwt.return () *)

(*     | Workflow (Extract _ as w) -> *)
(*       (fun () -> Path (Db.cache_path db w)), *)
(*       Lwt.return () *)

(*   and primitive_info_of_term : type s. s term -> primitive_info option = function *)
(*     | Prim (pi, _) -> Some pi *)
(*     | App (f, _, _) -> primitive_info_of_term f *)
(*     | _ -> None *)

(*   let eval : type s. s workflow -> s Lwt.t = fun w -> *)
(*     let path = Db.cache_path db w in *)
(*     let return_value : type s. s workflow -> s Lwt.t = function *)
(*       | Input (_, fn) -> Lwt.return (Path fn) *)
(*       | Extract _ -> Lwt.return (Path path) *)
(*       | Path_workflow (_, t) -> Lwt.return (Path path) *)
(*       | Value_workflow (_, t) -> *)
(*         Lwt_io.(with_file ~mode:Input path read_value) *)
(*     in *)
(*     build w >>= fun () -> return_value w *)

end
