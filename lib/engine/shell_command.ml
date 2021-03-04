open Core
open Bistro_internals

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type symbolic_file_dump = Symbolic_file_dump of {
    contents : Execution_env.insert Template.t ;
  }

type t = Command of {
    text : Execution_env.insert Command.t ;
    env : Execution_env.t ;
    container : [ `Docker_container of Workflow.Docker_image.t
                | `Singularity_container of Workflow.container_image ] option ;
  }

let container_env (Command { container ; env ; text = _ }) =
  match container with
  | None -> env
  | Some (`Docker_container _) -> Execution_env.dockerize env
  | Some (`Singularity_container _) -> Execution_env.singularize env

let rec file_dumps_of_tokens toks =
  List.map toks ~f:file_dumps_of_token
  |> List.concat
  |> List.dedup_and_sort ~compare:Caml.compare

and file_dumps_of_token =
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
    } :: file_dumps_of_tokens contents
    |> List.dedup_and_sort ~compare:Caml.compare

let rec file_dumps_of_command =
  let open Command in
  function
  | Simple_command toks -> file_dumps_of_tokens toks
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:file_dumps_of_command
    |> List.concat
    |> List.dedup_and_sort ~compare:Caml.compare

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

let compile_file_dump (env : Execution_env.t) (container_env : Execution_env.t) (Symbolic_file_dump { contents }) =
  let path = env.file_dump contents in
  let text = string_of_tokens container_env contents in
  File_dump { path ; text }

let file_dumps (Command cmd as c) =
  file_dumps_of_command cmd.text
  |> List.map ~f:(compile_file_dump cmd.env (container_env c))

let par x = "( " ^ x ^ " )"
(* spaces after '(' and before ')' are essential here, to prevent '((
   ))' which has a specific meaning for bash *)

let command_path_deps cmd =
  Command.deps cmd ~compare:Execution_env.compare_insert
  |> List.filter_map ~f:(function
      | Execution_env.Path p -> Some [ p ]
      | Path_list l -> Some l.elts
      | String _ -> None
    )
  |> List.concat

module Mounts = struct
  type t = {
      host_paths : string list ;
      container_paths : string list ;
    }

  let of_pair (host_paths, container_paths) = {
      host_paths ;
      container_paths ;
    }

  let deps ~env (Command cmd) =
    let open Execution_env in
    command_path_deps cmd.text
    |> List.map ~f:(Execution_env.container_mount env.db)
    |> List.map ~f:Execution_env.(fun m -> m.mount_host_location, m.mount_container_location)
    |> List.dedup_and_sort ~compare:Caml.compare
    |> List.unzip
    |> of_pair

  let file_dumps env container_env (Command cmd) =
    let open Execution_env in
    let file_dumps = file_dumps_of_command cmd.text in
    let f env (Symbolic_file_dump { contents = fd ; _ }) =
      env.file_dump fd
    in
    {
      host_paths = List.map file_dumps ~f:(f env) ;
      container_paths = List.map file_dumps ~f:(f container_env) ;
    }

  let tmp env container_env =
    let open Execution_env in
    {
      host_paths = [env.tmp] ;
      container_paths = [container_env.tmp] ;
    }

  let script script =
    {
      host_paths = [script] ;
      container_paths = [script] ;
    }

  let dest env container_env =
    let open Execution_env in
    {
      host_paths = Filename.[ dirname env.dest ] ;
      container_paths = Filename.[ dirname container_env.dest ] ;
    }

  let docker_opt { host_paths ; container_paths } =
    Docker.mount_options ~host_paths ~container_paths
end

let singularity_mounts (env : Execution_env.t) (container_env : Execution_env.t) cmd script_fn =
  let binding (m : Mounts.t) =
    List.map2_exn m.host_paths m.container_paths ~f:(fun hp cp ->
        sprintf "%s:%s" hp cp
      )
  in
  Mounts.[
      deps ~env cmd ;
      dest env container_env ;
      script script_fn ;
      file_dumps env container_env cmd ;
      tmp env container_env ;
  ]
  |> List.concat_map ~f:binding
  |> String.concat ~sep:","

let rec string_of_command env =
  let open Command in
  function
  | Simple_command tokens -> string_of_tokens env tokens
  | And_list xs -> par (string_of_command_aux env " && " xs)
  | Or_list xs -> par (string_of_command_aux env " || " xs)
  | Pipe_list xs -> par (string_of_command_aux env " | " xs)

and string_of_command_aux env sep xs =
  List.map xs ~f:(string_of_command env)
  |> String.concat ~sep

let text (Command cmd as c) =
  string_of_command (container_env c) cmd.text

let make env img cmd =
  let container = Execution_env.choose_container env.Execution_env.allowed_containers img in
  Command {
    text = cmd ;
    env ;
    container ;
  }

let write_file_dumps xs =
  let f (File_dump { text ; path }) =
    Lwt_io.(with_file ~mode:output path (fun oc -> write oc text))
  in
  Lwt_list.iter_p f xs

let uses_docker (Command cmd) = match cmd.container with
  | None | Some (`Singularity_container _) -> false
  | Some (`Docker_container _) -> true

let invocation (Command cmd as c) script =
  match cmd.container with
  | None -> sprintf "/bin/bash %s" script
  | Some (`Docker_container image) ->
     let container_env = container_env c in
     sprintf
       "docker run --log-driver=none --rm %s %s %s %s %s -i %s /bin/bash %s"
       (Mounts.deps ~env:cmd.env c |> Mounts.docker_opt)
       (Mounts.file_dumps cmd.env container_env c |> Mounts.docker_opt)
       (Mounts.tmp cmd.env container_env |> Mounts.docker_opt)
       (Mounts.dest cmd.env container_env |> Mounts.docker_opt)
       (Mounts.script script |> Mounts.docker_opt)
       (Docker.image_url image)
       script
  | Some (`Singularity_container image) ->
     let container_env = container_env c in
     sprintf
       "singularity exec --no-home -B %s %s /bin/bash '%s'"
       (singularity_mounts cmd.env container_env c script)
       (Db.singularity_image cmd.env.Execution_env.db image)
       script

let run (Command cmd as c) =
  let open Lwt in
  let script_file = Filename.concat cmd.env.tmp_dir "script.sh" in
  let invocation = invocation c script_file in
  Misc.remove_if_exists cmd.env.tmp_dir >>= fun () ->
  Unix.mkdir_p cmd.env.tmp ;
  Out_channel.write_all (Filename.concat cmd.env.tmp_dir "run.sh") ~data:invocation ;
  write_file_dumps (file_dumps c) >>= fun () ->
  Lwt_io.(with_file
            ~mode:output script_file
            (fun oc -> write oc (text c))) >>= fun () ->
  Misc.redirection cmd.env.stdout >>= fun stdout ->
  Misc.redirection cmd.env.stderr >>= fun stderr ->
  Lwt_process.exec ~stdout ~stderr (Lwt_process.shell invocation)
  >>= fun status ->
  (* Lwt_unix.unlink script_file >>= fun () -> *)
  let exit_code = Caml.Unix.(
      match status with
      | WEXITED code
      | WSIGNALED code
      | WSTOPPED code -> code
    )
  in
  let dest_exists = match Sys.file_exists cmd.env.dest with `Yes -> true | `Unknown | `No -> false in
  (
    if Execution_env.allows_docker cmd.env && uses_docker c then (
      Misc.docker_chown ~path:cmd.env.tmp_dir ~uid:cmd.env.uid >>= fun () ->
      if dest_exists then Misc.docker_chown ~path:cmd.env.dest ~uid:cmd.env.uid
      else Lwt.return ()
    )
    else Lwt.return ()
  ) >>= fun () ->
  Lwt.return (exit_code, dest_exists)

let container (Command cmd) = cmd.container
