open Core.Std
open Bistro

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )
let ( >>=? ) x f = x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let mv src dst =
  Lwt_process.exec ("", [| "mv" ; src ; dst |]) >>| ignore

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-rf" ; fn |]) >>| ignore
  else
    Lwt.return ()

module Pool : sig
  type t

  val create : np:int -> mem:int -> t
  val use : t -> np:int -> mem:int -> f:(np:int -> mem:int -> 'a Lwt.t) -> 'a Lwt.t
end =
struct

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
    let np = min np p.np in
    if mem > p.mem then
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


type error = (Task.dep * string) list
type 'a result = ('a, error) Result.t

type execution_report = {
  script : string ;
  exit_status : int ;
}

type backend =
  Db.t -> Task.t -> execution_report Lwt.t

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let interpreter_cmd path_to_script = function
  | `bash -> [ "bash" ; path_to_script ]
  | `ocaml -> [ "ocaml" ; path_to_script ]
  | `ocamlscript -> [ "ocamlscript" ; path_to_script ]
  | `python -> [ "python" ; path_to_script ]
  | `perl -> [ "perl" ; path_to_script ]
  | `R -> [ "Rscript" ; path_to_script ]
  | `sh -> [ "sh" ; path_to_script ]

let interpreter_cmd path_to_script interpreter =
  "", Array.of_list (interpreter_cmd path_to_script interpreter)

let extension_of_interpreter = function
  | `bash -> "sh"
  | `ocaml -> "ml"
  | `ocamlscript -> "ml"
  | `python -> "py"
  | `perl -> "pl"
  | `R -> "R"
  | `sh -> "sh"

let deps_of_template tmpl =
  let open Task in
  List.filter_map tmpl ~f:(function
      | D id -> Some id
      | S _ | DEST | TMP | NP | MEM -> None
    )
  |> List.dedup

let deps_of_simple_cmd cmd = deps_of_template cmd.Task.tokens

let deps_of_script script =
  List.append
    (deps_of_template script.Task.args)
    (deps_of_template script.Task.text)
  |> List.dedup

type execution_env = {
  use_docker : bool ;
  dest : string ;
  tmp : string ;
  script_tmp : Task.script -> string ;
  dep : Task.dep -> string ;
  np : int ;
  mem : int ;
}

let make_execution_env ~tmpdir ~use_docker ~np ~mem db task =
  let path_of_task_id id =
    Db.Task.cache_path db (Option.value_exn (Db.Task_table.get db id))
  in
  let dep = function
    | `Input p -> string_of_path p
    | `Task tid -> path_of_task_id tid
    | `Select (tid, p) ->
      Filename.concat (path_of_task_id tid) (string_of_path p)
  in
  {
    use_docker ;
    tmp = Filename.concat tmpdir "tmp" ;
    dest = Filename.concat tmpdir "dest" ;
    script_tmp = (fun s -> Filename.concat tmpdir (digest s)) ;
    dep ;
    np ;
    mem ;
  }

let make_docker_execution_env env = {
  use_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = (fun d -> sprintf "/bistro/data/%s" (digest d)) ;
  script_tmp = (fun s -> Filename.concat "/bistro/script" (digest s)) ;
  np = env.np ;
  mem = env.mem ;
}

module Concrete_task = struct
  type t =
    | Sh of string
    | Run_script of run_script
    | And of t list
    | Or of t list
    | Pipe of t list

  and run_script = {
    cmd  : string ;
    text : string ;
    path : string ;
  }


  let docker_image_url image =
    sprintf "%s%s/%s%s"
      (Option.value_map ~default:"" ~f:(sprintf "%s/") image.dck_registry)
      image.dck_account
      image.dck_name
      (Option.value_map ~default:"" ~f:(sprintf ":%s")  image.dck_tag)

  let token env =
    let open Task in
    function
    | S s -> s
    | D d -> env.dep d
    | DEST -> env.dest
    | TMP -> env.tmp
    | NP -> string_of_int env.np
    | MEM -> string_of_int env.mem

  let tokens env xs =
    List.map ~f:(token env) xs
    |> String.concat

  let digest x =
    Digest.to_hex (Digest.string (Marshal.to_string x []))

  let deps_mount env dck_env deps =
    let f d = sprintf "-v %s:%s" (env.dep d) (dck_env.dep d) in
    List.map deps ~f
    |> String.concat ~sep:" "

  let tmp_mount env dck_env =
    sprintf "-v %s:%s" env.tmp dck_env.tmp

  let dest_mount env dck_env =
    sprintf "-v %s:%s" Filename.(dirname env.dest) Filename.(dirname dck_env.dest)

  let rec simple_cmd env cmd =
    let open Task in
    match env.use_docker, cmd.env with
    | true, Some image ->
      let dck_env = make_docker_execution_env env in
      Sh (
        sprintf
          "docker run %s %s %s -t %s sh -c \"%s\""
          (deps_mount env dck_env (deps_of_simple_cmd cmd))
          (tmp_mount env dck_env)
          (dest_mount env dck_env)
          (docker_image_url image)
          (tokens (make_docker_execution_env env) cmd.tokens))
    | _ ->
      Sh (tokens env cmd.tokens)

  let rec run_script env script =
    let make_script ~cmd ~text =
      Run_script {
        cmd ; text ; path = env.script_tmp script
      }
    in
    let script_invokation env =
      sprintf "%s %s %s"
        script.Task.interpreter
        (env.script_tmp script)
        (tokens env script.Task.args)
    in
    match env.use_docker, script.Task.script_env with
    | true, Some image ->
      let dck_env = make_docker_execution_env env in
      let script_mount = sprintf "-v %s:%s" (env.script_tmp script) (dck_env.script_tmp script) in
      let text = tokens dck_env script.Task.text in
      let cmd =
        sprintf
          "docker run %s %s %s %s -t %s sh -c \"%s\""
          (deps_mount env dck_env (deps_of_script script))
          (tmp_mount env dck_env)
          (dest_mount env dck_env)
          script_mount
          (docker_image_url image)
          (script_invokation dck_env)
      in
      make_script ~text ~cmd

    | _ ->
      let text = tokens env script.Task.text in
      let cmd = script_invokation env in
      make_script ~text ~cmd

  let rec any env =
    let open Task in
    function
    | And_sequence xs -> And (sequence env " && " xs)
    | Or_sequence xs -> Or (sequence env " || " xs)
    | Pipe_sequence xs -> Pipe (sequence env " | " xs)
    | Simple_command cmd -> simple_cmd env cmd
    | Run_script s -> run_script env s

  and sequence env sep xs = List.map xs ~f:(any env)

  let of_task_cmd env cmd = any env cmd

  let rec scripts = function
    | And xs
    | Or xs
    | Pipe xs ->
      List.concat (List.map xs ~f:scripts)
    | Sh _ -> []
    | Run_script s -> [ s ]

  let par x = "(" ^ x ^ ")"

  let par_if_necessary x y =
    match x with
    | Run_script _
    | Sh _ -> y
    | And _
    | Or _
    | Pipe _ -> par y

  let rec to_cmd = function
    | And xs -> to_cmd_aux " && " xs
    | Or xs -> to_cmd_aux " || " xs
    | Pipe xs -> to_cmd_aux " | " xs
    | Sh cmd -> cmd
    | Run_script s -> s.cmd

  and to_cmd_aux sep xs =
    List.map xs ~f:to_cmd
    |> List.map2_exn ~f:par_if_necessary xs
    |> String.concat ~sep

end

let write_scripts ctask =
  let scripts = Concrete_task.scripts ctask in
  List.iter scripts ~f:(fun { Concrete_task.path ; text } ->
      Out_channel.write_all path ~data:text
    )

let local_backend ?tmpdir ?(use_docker = false) ~np ~mem () : backend =
  let open Task in
  let pool = Pool.create ~np ~mem in
  let uid = Unix.getuid () in
  fun db ({ cmd ; np ; mem } as task) ->
    Pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
        let tmpdir = match tmpdir with
          | None -> Db.Task.tmp_path db task
          | Some p -> Filename.concat p task.id in
        let stdout = Db.Task.stdout_path db task in
        let stderr = Db.Task.stderr_path db task in
        let env = make_execution_env ~tmpdir ~use_docker ~np ~mem db task in
        let ctask = Concrete_task.of_task_cmd env cmd in
        remove_if_exists tmpdir >>= fun () ->
        Unix.mkdir_p env.tmp ;
        write_scripts ctask ;
        let script_file =
          Filename.temp_file "guizmin" ".sh" in
        let script_text = Concrete_task.to_cmd ctask in
        Lwt_io.(with_file
                  ~mode:output script_file
                  (fun oc -> write oc script_text)) >>= fun () ->
        redirection stdout >>= fun stdout ->
        redirection stderr >>= fun stderr ->
        let cmd = interpreter_cmd script_file `sh in
        Lwt_process.exec ~stdout ~stderr cmd >>= fun status ->
        if use_docker then ( (* FIXME: not necessary if no docker command was run *)
          sprintf "docker run -v %s:/bistro -t busybox chown -R %d /bistro" tmpdir uid
          |> Sys.command
          |> ignore
        ) ;
        let exit_status = Caml.Unix.(match status with
            | WEXITED code
            | WSIGNALED code
            | WSTOPPED code -> code
          )
        in
        let dest_exists = Sys.file_exists env.dest = `Yes in
        (
          if dest_exists then
            mv env.dest (Db.Task.build_path db task)
          else
            Lwt.return ()
        ) >>= fun () ->
        Lwt_unix.unlink script_file >>= fun () ->
        (
          if exit_status = 0 && dest_exists then
            remove_if_exists tmpdir
          else
            Lwt.return ()
        ) >>= fun () ->
        Lwt.return {
          script = script_text ;
          exit_status ;
        }
      )


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
  val find_or_add : t -> Task.t -> (unit -> unit result Lwt.t) -> unit result Lwt.t
  val join : t -> unit Lwt.t
end
=
struct
  module S = struct
    open Task
    type t = Task.t
    let equal x y = x.id = y.id
    let hash x = String.hash x.id
  end

  module T = Caml.Hashtbl.Make(S)

  type contents =
    | Thread of unit result Lwt.t

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
          Lwt.wakeup u res ;
          Lwt.return ()
        ) ;
      waiter

  let join table =
    let f _ (Thread t) accu = (Lwt.map ignore t) :: accu in
    T.fold f table []
    |> Lwt.join
end


type t = {
  db : Db.t ;
  backend : backend ;
  cbs : CBST.t ;
  mutable on : bool ;
}

let make backend db = {
  db ;
  backend = backend ;
  cbs = CBST.create () ;
  on = true ;
}

let join_results xs =
  let f accu x =
    x >>= function
    | Ok () -> Lwt.return accu
    | Error errors as e ->
      match accu with
      | Ok _ -> Lwt.return e
      | Error errors' -> Lwt.return (Error (errors @ errors'))
  in
  Lwt_list.fold_left_s f (Ok ()) xs


let rec build_dep e = function
  | `Input i -> (build_input e i : _ Lwt.t)
  | `Select (sel_from, sel_path) -> build_select e sel_from sel_path
  | `Task id ->
    let task = Option.value_exn (Db.Task_table.get e.db id) in
    Db.Task.requested e.db task ;
    if Db.Task.in_cache e.db task then
      Lwt.return (Ok ())
    else
      CBST.find_or_add e.cbs task (fun () ->
          let dep_threads = List.map task.Task.deps ~f:(build_dep e) in
          build_task e task dep_threads
        )

and build_task
    e
    ({ Task.np ; mem ; cmd } as task)
    dep_threads =

  join_results dep_threads >>=? fun () ->
  (
    let stdout = Db.Task.stdout_path e.db task in
    let stderr = Db.Task.stderr_path e.db task in
    let build_path = Db.Task.build_path e.db task in
    remove_if_exists stdout >>= fun () ->
    remove_if_exists stderr >>= fun () ->
    remove_if_exists build_path >>= fun () ->
    e.backend e.db task >>= fun { script ; exit_status } ->
    Db.Submitted_script_table.set e.db task script ;
    match exit_status, Sys.file_exists build_path = `Yes with
    | 0, true ->
      Db.Task.built e.db task ;
      mv build_path (Db.Task.cache_path e.db task) >>= fun () ->
      Lwt.return (Ok ())
    | 0, false ->
      let msg =
        "Workflow failed to produce its output at the prescribed location."
      in
      Lwt.return (Error [ `Task task.Task.id, msg ])
    | error_code, _ ->
      let msg = sprintf "Script exited with code %d" error_code in
      Lwt.return (Error [ `Task task.Task.id, msg ])
  )

and build_input e input_path =
  Lwt.wrap (fun () ->
      let p = string_of_path input_path in
      if Sys.file_exists p <> `Yes then
        let msg =
          sprintf
            "File %s is declared as an input of a workflow but does not exist."
            p
        in
        Error [ `Input input_path, msg ]
      else
        Ok ()
    )

and build_select e sel_from sel_path =
  let p = string_of_path sel_path in
  let dir = Option.value_exn (Db.Task_table.get e.db sel_from) in
  let dir_path = Db.Task.cache_path e.db dir in
  let check_in_dir () =
    if Sys.file_exists (Filename.concat dir_path p) <> `Yes
    then (
      let msg =
        sprintf "No file or directory named %s in directory workflow %s."
          p
          dir_path
      in
      Lwt.return (Error [ `Select (sel_from, sel_path), msg ])
    )
    else Lwt.return (Ok ())
  in
  if Sys.file_exists dir_path = `Yes then (
    check_in_dir () >>=? fun () ->
    Db.Task.requested e.db dir ;
    Lwt.return (Ok ())
  )
  else (
    let dir_thread = build_dep e (`Task sel_from) in
    dir_thread >>=? check_in_dir
  )


let build e w =
  (
    if e.on then (
      Db.register_workflow e.db w ;
      build_dep e (Task.classify_workflow w)
    )
    else
      Lwt.fail (Invalid_argument "Engine_halted")
  )
  >>= function
  | Ok () -> Lwt.return (Ok (Db.workflow_path e.db w))
  | Error xs ->
    Lwt.return (Error xs)

let title_of_dep = function
  | `Task tid -> tid
  | `Select (tid, p) -> Filename.concat tid (string_of_path p)
  | `Input i -> string_of_path i

let build_exn e w =
  build e w >>= function
  | Ok s -> Lwt.return s
  | Error xs ->
    let msgs = List.map ~f:(fun (dep, msg) -> (title_of_dep dep) ^ "\t" ^ msg) xs in
    let msg = sprintf "Some build(s) failed:\n\t%s\n" (String.concat ~sep:"\n\t" msgs) in
    Lwt.fail (Failure msg)

let shutdown e =
  e.on <- false ;
  CBST.join e.cbs
