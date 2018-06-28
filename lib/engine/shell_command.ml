open Core
open Bistro_base

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }


type symbolic_file_dump = Symbolic_file_dump of {
    contents : string Template.t ;
    in_docker : bool ;
  }

type t = Command of {
    text : string ;
    file_dumps : file_dump list ;
    env : Execution_env.t ;
    uses_docker : bool ;
  }

let text (Command cmd) = cmd.text
let file_dumps (Command cmd) = cmd.file_dumps

let rec file_dumps_of_tokens in_docker toks =
  List.map toks ~f:(file_dumps_of_token in_docker)
  |> List.concat
  |> List.dedup_and_sort ~compare:Caml.compare

and file_dumps_of_token in_docker =
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
      in_docker = in_docker
    } :: file_dumps_of_tokens in_docker contents
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
  | Docker (_, cmd) -> file_dumps_of_command true cmd

let string_of_token (env : Execution_env.t) =
  let open Template in
  function
  | S s -> s
  | D dep -> (* env.dep *) dep
  | F toks -> Workflow.digest toks
  | DEST -> env.dest
  | TMP -> env.tmp
  | NP -> string_of_int env.np
  | MEM -> string_of_int env.mem

let string_of_tokens env xs =
  List.map ~f:(string_of_token env) xs
  |> String.concat

let par x = "(" ^ x ^ ")"


(* let deps_mount ~env ~dck_env deps =
 *   let open Execution_env in
 *   Docker.mount_options
 *     ~host_paths:(List.map deps ~f:env.dep)
 *     ~container_paths:(List.map deps ~f:dck_env.dep) *)

let cache_mount env =
  Docker.mount_options
    ~host_paths:[Db.cache_dir env.Execution_env.db]
    ~container_paths:[Execution_env.docker_cache_dir]

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


let rec string_of_command env =
  let open Command in
  function
  | Simple_command tokens -> string_of_tokens env tokens
  | And_list xs -> par (string_of_command_aux env " && " xs)
  | Or_list xs -> par (string_of_command_aux env " || " xs)
  | Pipe_list xs -> par (string_of_command_aux env " | " xs)
  | Docker (image, cmd) ->
    if env.using_docker then
      let dck_env = Execution_env.dockerize env in
      sprintf
        "docker run --log-driver=none --rm %s %s %s %s -i %s bash -c '%s'"
        (cache_mount env)
        (file_dumps_mount env dck_env (file_dumps_of_command true cmd))
        (tmp_mount env dck_env)
        (dest_mount env dck_env)
        (Docker.image_url image)
        (string_of_command dck_env cmd)
    else
      string_of_command env cmd

and string_of_command_aux env sep xs =
  List.map xs ~f:(string_of_command env)
  |> String.concat ~sep

let compile_file_dump env (Symbolic_file_dump { contents ; in_docker }) =
  let exec_env =
    if in_docker && env.Execution_env.using_docker
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
    uses_docker = Command.uses_docker cmd ;
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
    if cmd.env.using_docker && cmd.uses_docker then (
      Misc.docker_chown ~path:cmd.env.tmp_dir ~uid:cmd.env.uid >>= fun () ->
      if dest_exists then Misc.docker_chown ~path:cmd.env.dest ~uid:cmd.env.uid
      else Lwt.return ()
    )
    else Lwt.return ()
  ) >>= fun () ->
  Lwt.return (exit_code, dest_exists)
