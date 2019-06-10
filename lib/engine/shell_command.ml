open Core
open Bistro_internals

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type symbolic_file_dump = Symbolic_file_dump of {
    contents : Execution_env.insert Template.t ;
    in_docker_container : bool ;
  }

type t = Command of {
    text : string ;
    file_dumps : file_dump list ;
    env : Execution_env.t ;
    uses_docker : bool ; (* only used to chown results when done through docker *)
  }

let text (Command cmd) = cmd.text
let file_dumps (Command cmd) = cmd.file_dumps

let rec file_dumps_of_tokens in_docker toks =
  List.map toks ~f:(file_dumps_of_token in_docker)
  |> List.concat
  |> List.dedup_and_sort ~compare:Caml.compare

and file_dumps_of_token in_docker_container =
  let open Template in
  function
  | NP
  | DEST
  | TMP
  | S _
  | D _
  | MEM -> []
  | F contents ->
    Symbolic_file_dump {
      contents = contents ;
      in_docker_container ;
    } :: file_dumps_of_tokens in_docker_container contents
    |> List.dedup_and_sort ~compare:Caml.compare

let rec file_dumps_of_command in_docker =
  let open Command in
  function
  | Simple_command toks -> file_dumps_of_tokens in_docker toks
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:(file_dumps_of_command in_docker)
    |> List.concat
    |> List.dedup_and_sort ~compare:Caml.compare
  | Within_env (_, cmd) -> file_dumps_of_command true cmd

let string_of_token (env : Execution_env.t) =
  let open Template in
  function
  | S s -> s
  | D (Execution_env.Path p) -> env.dep p
  | D (Execution_env.Path_list { elts ; quote ; sep }) ->
    let quote =
      Option.value_map quote ~default:Fn.id ~f:(fun c p -> sprintf "%c%s%c" c p c)
    in
    List.map elts ~f:env.dep
    |> List.map ~f:quote
    |> String.concat ~sep
  | D (String s) -> s
  | F toks -> env.file_dump toks
  | DEST -> env.dest
  | TMP -> env.tmp
  | NP -> string_of_int env.np
  | MEM -> string_of_int env.mem

let string_of_tokens env xs =
  List.map ~f:(string_of_token env) xs
  |> String.concat

let par x = "(" ^ x ^ ")"

let deps_mount ~env deps =
  let open Execution_env in
  let mounts = List.map deps ~f:(Execution_env.container_mount env.db) in
  let host_paths, container_paths =
    List.map mounts ~f:Execution_env.(fun m -> m.mount_host_location, m.mount_container_location)
    |> List.dedup_and_sort ~compare:Caml.compare
    |> List.unzip
  in
  Docker.mount_options ~host_paths ~container_paths

(* let cache_mount env =
 *   Docker.mount_options
 *     ~host_paths:[Db.cache_dir env.Execution_env.db]
 *     ~container_paths:[Execution_env.docker_cache_dir] *)

let file_dumps_mount env dck_env file_dumps =
  let open Execution_env in
  let f env (Symbolic_file_dump { contents = fd ; _ }) =
    env.file_dump fd
  in
  Docker.mount_options
    ~host_paths:(List.map file_dumps ~f:(f env))
    ~container_paths:(List.map file_dumps ~f:(f dck_env))

let tmp_mount env dck_env =
  let open Execution_env in
  Docker.mount_options
    ~host_paths:[env.tmp]
    ~container_paths:[dck_env.tmp]

let dest_mount env dck_env =
  let open Execution_env in
  Docker.mount_options
    ~host_paths:Filename.[ dirname env.dest ]
    ~container_paths:Filename.[ dirname dck_env.dest ]


let command_path_deps cmd =
  Command.deps cmd
  |> List.filter_map ~f:(function
      | Execution_env.Path p -> Some [ p ]
      | Path_list l -> Some l.elts
      | String _ -> None
    )
  |> List.concat

let rec string_of_command env =
  let open Command in
  function
  | Simple_command tokens -> string_of_tokens env tokens
  | And_list xs -> par (string_of_command_aux env " && " xs)
  | Or_list xs -> par (string_of_command_aux env " || " xs)
  | Pipe_list xs -> par (string_of_command_aux env " | " xs)
  | Within_env (img, cmd) ->
    match Execution_env.choose_environment env.Execution_env.allowed_environments img with
    | `Plain ->
      string_of_command env cmd
    | `Docker_container image ->
      let dck_env = Execution_env.dockerize env in
      sprintf
        "docker run --log-driver=none --rm %s %s %s %s -i %s bash -c '%s'"
        (deps_mount ~env (command_path_deps cmd))
        (file_dumps_mount env dck_env (file_dumps_of_command true cmd))
        (tmp_mount env dck_env)
        (dest_mount env dck_env)
        (Docker.image_url image)
        (string_of_command dck_env cmd)
    | `Singularity_container img ->
      let env = Execution_env.singularize env in
      sprintf
        "singularity exec %s bash -c '%s'"
        (Db.singularity_image env.Execution_env.db img)
        (string_of_command env cmd)
    | `Guix gui_env ->
      let pkgs = List.map gui_env ~f:(fun p -> sprintf "%s@%s" p.name p.version) in
      sprintf "guix environment --ad-hoc %s -- %s"
        (String.concat ~sep:" " pkgs)
        (string_of_command env cmd)

and string_of_command_aux env sep xs =
  List.map xs ~f:(string_of_command env)
  |> String.concat ~sep

let rec command_uses_docker env = function
  | Command.Simple_command _ -> false
  | And_list xs
  | Or_list xs
  | Pipe_list xs -> command_uses_docker_aux env xs
  | Within_env (img, _) ->
    match Execution_env.choose_environment env.Execution_env.allowed_environments img with
    | `Plain
    | `Guix _
    | `Singularity_container _ -> false
    | `Docker_container _ -> true
and command_uses_docker_aux env xs =
  List.exists xs ~f:(command_uses_docker env)

let compile_file_dump env (Symbolic_file_dump { contents ; in_docker_container }) =
  let exec_env =
    if in_docker_container && Execution_env.allows_docker env
    then Execution_env.dockerize env
    else env
  in
  let path = env.file_dump contents in
  let text = string_of_tokens exec_env contents in
  File_dump { path ; text }

let make env cmd =
  Command {
    text = string_of_command env cmd ;
    file_dumps =
      file_dumps_of_command false cmd
      |> List.map ~f:(compile_file_dump env) ;
    env ;
    uses_docker = command_uses_docker env cmd ;
  }

let write_file_dumps xs =
  let f (File_dump { text ; path }) =
    Lwt_io.(with_file ~mode:output path (fun oc -> write oc text))
  in
  Lwt_list.iter_p f xs

let run (Command cmd) =
  let open Lwt in
  let script_file = Filename.temp_file "guizmin" ".sh" in
  Misc.remove_if_exists cmd.env.tmp_dir >>= fun () ->
  Unix.mkdir_p cmd.env.tmp ;
  write_file_dumps cmd.file_dumps >>= fun () ->
  Lwt_io.(with_file
            ~mode:output script_file
            (fun oc -> write oc cmd.text)) >>= fun () ->
  Misc.redirection cmd.env.stdout >>= fun stdout ->
  Misc.redirection cmd.env.stderr >>= fun stderr ->
  Lwt_process.exec ~stdout ~stderr ("", [| "sh" ; script_file |])
  >>= fun status ->
  Lwt_unix.unlink script_file >>= fun () ->
  let exit_code = Caml.Unix.(
      match status with
      | WEXITED code
      | WSIGNALED code
      | WSTOPPED code -> code
    )
  in
  let dest_exists = Sys.file_exists cmd.env.dest = `Yes in
  (
    if Execution_env.allows_docker cmd.env && cmd.uses_docker then (
      Misc.docker_chown ~path:cmd.env.tmp_dir ~uid:cmd.env.uid >>= fun () ->
      if dest_exists then Misc.docker_chown ~path:cmd.env.dest ~uid:cmd.env.uid
      else Lwt.return ()
    )
    else Lwt.return ()
  ) >>= fun () ->
  Lwt.return (exit_code, dest_exists)
