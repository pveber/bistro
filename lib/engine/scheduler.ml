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

let rec deps_of_command =
  let open Task in
  function
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:deps_of_command
    |> List.concat
    |> List.dedup
  | Simple_command tokens -> deps_of_template tokens
  | Docker (_, c) -> deps_of_command c

type execution_env = {
  use_docker : bool ;
  dest : string ;
  tmp : string ;
  dep : Task.dep -> string ;
  np : int ;
  mem : int ;
}

let make_execution_env ~tmpdir ~use_docker ~np ~mem db =
  let path_of_task_id id =
    Db.Task.cache_path db (Option.value_exn (Db.Task_table.get db id))
  in
  let dep = function
    | `Input p ->
      let p = string_of_path p in
      if Filename.is_relative p then
        Filename.concat (Sys.getcwd ()) p
      else
        p
    | `Task tid -> path_of_task_id tid
    | `Select (tid, p) ->
      Filename.concat (path_of_task_id tid) (string_of_path p)
  in
  {
    use_docker ;
    tmp = Filename.concat tmpdir "tmp" ;
    dest = Filename.concat tmpdir "dest" ;
    dep ;
    np ;
    mem ;
  }

let make_docker_execution_env env = {
  use_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = (fun d -> sprintf "/bistro/data/%s" (digest d)) ;
  np = env.np ;
  mem = env.mem ;
}

module Concrete_task = struct
  type t = instruction list
  and instruction =
    | Sh of string
    | Dump of dump

  and dump = {
    dump_dest : string  ;
    dump_contents : string ;
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

  let string_of_tokens env xs =
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

  let par x = "(" ^ x ^ ")"

  let rec string_of_command env =
    let open Task in
    function
    | Simple_command tokens -> string_of_tokens env tokens
    | And_list xs -> par (string_of_command_aux env " && " xs)
    | Or_list xs -> par (string_of_command_aux env " || " xs)
    | Pipe_list xs -> par (string_of_command_aux env " | " xs)
    | Docker (image, cmd) ->
      if env.use_docker then
        let dck_env = make_docker_execution_env env in
        sprintf
          "docker run %s %s %s -t %s bash -c \"%s\""
          (deps_mount env dck_env (deps_of_command cmd))
          (tmp_mount env dck_env)
          (dest_mount env dck_env)
          (docker_image_url image)
          (string_of_command (make_docker_execution_env env) cmd)
      else
        string_of_command env cmd

  and string_of_command_aux env sep xs =
    List.map xs ~f:(string_of_command env)
    |> String.concat ~sep

  let dump env { Task.dest ; contents } =
    Dump {
      dump_dest = string_of_tokens env dest ;
      dump_contents = string_of_tokens env contents ;
    }

  let of_instruction env = function
    | Task.Dump d -> Dump {
        dump_dest = string_of_tokens env d.Task.dest ;
        dump_contents = string_of_tokens env d.Task.contents ;
      }
    | Task.Sh cmd -> Sh (string_of_command env cmd)

  let perform ~stdout ~stderr = function
    | Sh cmd ->
      let script_file = Filename.temp_file "guizmin" ".sh" in
      let script_text = cmd in
      Lwt_io.(with_file
                ~mode:output script_file
                (fun oc -> write oc script_text)) >>= fun () ->
      redirection stdout >>= fun stdout ->
      redirection stderr >>= fun stderr ->
      let cmd = "", [| "sh" ; script_file |] in
      Lwt_process.exec ~stdout ~stderr cmd >>= fun status ->
      Lwt_unix.unlink script_file >>| fun () ->
      Caml.Unix.(match status with
          | WEXITED code
          | WSIGNALED code
          | WSTOPPED code -> code
        )

    | Dump { dump_dest ; dump_contents } ->
      Lwt_io.(with_file
                ~mode:output dump_dest
                (fun oc -> write oc dump_contents))
      >>| fun _ -> 0


end

let run_program ~stdout ~stderr env instructions =
  let rec loop code = function
    | [] -> Lwt.return code
    | h :: t ->
      Concrete_task.(perform ~stdout ~stderr (of_instruction env h)) >>= fun code ->
      if code = 0 then loop code t
      else Lwt.return code
  in
  loop 0 instructions

let local_backend ?tmpdir ?(use_docker = false) ~np ~mem () : backend =
  let open Task in
  let pool = Pool.create ~np ~mem in
  let uid = Unix.getuid () in
  fun db ({ program ; np ; mem } as task) ->
    Pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
        let tmpdir = match tmpdir with
          | None -> Db.Task.tmp_path db task
          | Some p -> Filename.concat p task.id in
        let stdout = Db.Task.stdout_path db task in
        let stderr = Db.Task.stderr_path db task in
        let env = make_execution_env ~tmpdir ~use_docker ~np ~mem db in
        remove_if_exists tmpdir >>= fun () ->
        Unix.mkdir_p env.tmp ;

        run_program ~stdout ~stderr env program >>= fun exit_status ->

        if use_docker then ( (* FIXME: not necessary if no docker command was run *)
          sprintf "docker run -v %s:/bistro -t busybox chown -R %d /bistro" tmpdir uid
          |> Sys.command
          |> ignore
        ) ;
        let dest_exists = Sys.file_exists env.dest = `Yes in
        (
          if dest_exists then
            mv env.dest (Db.Task.build_path db task)
          else
            Lwt.return ()
        ) >>= fun () ->
        (
          if exit_status = 0 && dest_exists then
            remove_if_exists tmpdir
          else
            Lwt.return ()
        ) >>= fun () ->
        Lwt.return {
          script = "" ; (* FIXME: there's no script to output anymore! *)
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
    ({ Task.np ; mem ; program } as task)
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

let build_all e ws =
  (
    if e.on then (
      Db.register_workflows e.db ws ;
      List.map ws ~f:Task.classify_any_workflow
      |> Lwt_list.map_p (build_dep e)
    )
    else
      Lwt.fail (Invalid_argument "Engine_halted")
  )
  >>|
  List.map2_exn ws ~f:(fun (Workflow w) -> function
      | Ok () -> Ok (Db.workflow_path e.db w)
      | Error xs -> Error xs
    )


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
