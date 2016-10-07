open Core.Std
open Rresult

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

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

type t =
  | Input of string * path
  | Select of string * [`Input of path | `Step of string] * path
  | Step of step

and step = {
  id      : id ;
  descr   : string ;
  deps    : dep list ;
  cmd     : command ;
  np      : int ; (** Required number of processors *)
  mem     : int ; (** Required memory in MB *)
  timeout : int option ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
}

and dep = [
    `Task of id
  | `Select of id * path
  | `Input of path
]
and id = string

and command =
  | Docker of Bistro.docker_image * command
  | Simple_command of token list
  | And_list of command list
  | Or_list of command list
  | Pipe_list of command list

and token =
  | S of string
  | D of dep
  | F of token list
  | DEST
  | TMP
  | NP
  | MEM

and path = string list
[@@deriving sexp]


let rec deps_of_template tmpl =
  List.map tmpl ~f:(function
      | D r -> [ r ]
      | F toks -> deps_of_template toks
      | S _ | DEST | TMP | NP | MEM -> []
    )
  |> List.concat
  |> List.dedup

let rec deps_of_command =
  function
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:deps_of_command
    |> List.concat
    |> List.dedup
  | Simple_command tokens -> deps_of_template tokens
  | Docker (_, c) -> deps_of_command c


type config = {
  db : Db.t ;
  use_docker : bool ;
}

let config ~db_path ~use_docker = {
  db = Db.init_exn db_path ;
  use_docker ;
}


let id = function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id




let denormalize_dep =
  let open Bistro in
  function
  | Step s -> `Task s.id
  | Input (_, p) -> `Input p
  | Select (_, Input (_, p), q) ->
    `Input (p @ q)
  | Select (_, Step s, p) ->
    `Select (s.id, p)
  | Select (_, Select _, _) -> assert false

let rec denormalize_token = function
  | Bistro.S s -> S s
  | Bistro.DEST -> DEST
  | Bistro.TMP -> TMP
  | Bistro.NP -> NP
  | Bistro.MEM -> MEM
  | Bistro.D d -> D (denormalize_dep d)
  | Bistro.F toks -> F (List.map toks ~f:denormalize_token)

let denormalize_template tmpl =
  List.map tmpl ~f:denormalize_token

let rec denormalize_cmd = function
  | Bistro.Simple_command tokens ->
    let tokens = denormalize_template tokens in
    Simple_command tokens
  | Bistro.And_list xs -> And_list (List.map xs ~f:denormalize_cmd)
  | Bistro.Or_list xs -> Or_list (List.map xs ~f:denormalize_cmd)
  | Bistro.Pipe_list xs -> Pipe_list (List.map xs ~f:denormalize_cmd)
  | Bistro.Docker (image, c) ->
    Docker (image, denormalize_cmd c)

let of_step { Bistro.id ; mem ; np ; descr ; cmd ; deps ; timeout ; version } =
  Step {
    id ;
    descr ;
    np ;
    mem ;
    cmd = denormalize_cmd cmd ;
    deps = List.map deps ~f:denormalize_dep ;
    timeout ;
    version ;
  }

let of_workflow = function
  | Bistro.Input (id, p) -> Input (id, p)
  | Bistro.Select (id, Bistro.Step { Bistro.id = dir_id }, p) ->
    Select (id, `Step dir_id, p)
  | Bistro.Select (id, Bistro.Input (_, dir_p), p) ->
    Select (id, `Input dir_p, p)
  | Bistro.Select (_, Bistro.Select _, _) -> assert false
  | Bistro.Step s -> of_step s

let requirement = function
  | Input _
  | Select _ -> Allocator.Request { np = 0 ; mem = 0 }
  | Step { np ; mem } ->
    Allocator.Request { np ; mem }


type execution_env = {
  use_docker : bool ;
  tmp_dir : string ;
  dest : string ;
  tmp : string ;
  dep : dep -> string ;
  file_dump : token list -> string ;
  np : int ;
  mem : int ;
}

let make_execution_env { db ; use_docker } ~np ~mem step =
  let tmp_dir = Db.tmp db step.id in
  let path_of_task_id tid = Db.cache db tid in
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
  let file_dump toks =
    Filename.concat tmp_dir (digest toks)
  in
  {
    tmp_dir ;
    use_docker ;
    tmp = Filename.concat tmp_dir "tmp" ;
    dest = Filename.concat tmp_dir "dest" ;
    file_dump ;
    dep ;
    np ;
    mem ;
  }

let make_docker_execution_env env = {
  tmp_dir = "/bistro" ;
  use_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = (fun d -> sprintf "/bistro/data/%s" (digest d)) ;
  file_dump = (fun toks -> sprintf "/bistro/data/%s" (digest toks)) ;
  np = env.np ;
  mem = env.mem ;
}

module Concrete_task = struct
  type t = Sh of string

  let docker_image_url image =
    sprintf "%s%s/%s%s"
      (Option.value_map ~default:"" ~f:(sprintf "%s/") image.Bistro.dck_registry)
      image.Bistro.dck_account
      image.Bistro.dck_name
      (Option.value_map ~default:"" ~f:(sprintf ":%s")  image.Bistro.dck_tag)

  let rec file_dumps_of_tokens in_docker toks =
    List.map toks ~f:(file_dumps_of_token in_docker)
    |> List.concat
    |> List.dedup

  and file_dumps_of_token in_docker =
    function
    | NP
    | DEST
    | TMP
    | S _
    | D _
    | MEM -> []
    | F f ->
      (`File_dump (f, in_docker)) :: file_dumps_of_tokens in_docker f
      |> List.dedup

  let rec file_dumps_of_command in_docker =
    function
    | Simple_command toks -> file_dumps_of_tokens in_docker toks
    | And_list xs
    | Or_list xs
    | Pipe_list xs ->
      List.map xs ~f:(file_dumps_of_command in_docker)
      |> List.concat
      |> List.dedup
    | Docker (_, cmd) -> file_dumps_of_command true cmd

  let token env =
    function
    | S s -> s
    | D d -> env.dep d
    | F toks -> env.file_dump toks
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

  let file_dumps_mount env dck_env file_dumps =
    let f (`File_dump (fd, _)) = sprintf "-v %s:%s" (env.file_dump fd) (dck_env.file_dump fd) in
    List.map file_dumps ~f
    |> String.concat ~sep:" "

  let tmp_mount env dck_env =
    sprintf "-v %s:%s" env.tmp dck_env.tmp

  let dest_mount env dck_env =
    sprintf "-v %s:%s" Filename.(dirname env.dest) Filename.(dirname dck_env.dest)

  let par x = "(" ^ x ^ ")"

  let rec string_of_command env =
    function
    | Simple_command tokens -> string_of_tokens env tokens
    | And_list xs -> par (string_of_command_aux env " && " xs)
    | Or_list xs -> par (string_of_command_aux env " || " xs)
    | Pipe_list xs -> par (string_of_command_aux env " | " xs)
    | Docker (image, cmd) ->
      if env.use_docker then
        let dck_env = make_docker_execution_env env in
        sprintf
          "docker run %s %s %s %s -t %s bash -c \"%s\""
          (deps_mount env dck_env (deps_of_command cmd))
          (file_dumps_mount env dck_env (file_dumps_of_command true cmd))
          (tmp_mount env dck_env)
          (dest_mount env dck_env)
          (docker_image_url image)
          (string_of_command (make_docker_execution_env env) cmd)
      else
        string_of_command env cmd

  and string_of_command_aux env sep xs =
    List.map xs ~f:(string_of_command env)
    |> String.concat ~sep

  let of_cmd env cmd = Sh (string_of_command env cmd)

  let generate_file_dumps env cmd =
    let file_dumps = file_dumps_of_command false cmd in
    let f (`File_dump (toks, in_docker)) =
      let exec_env = if in_docker then make_docker_execution_env env else env in
      let path = env.file_dump toks in
      let text = string_of_tokens exec_env toks in
      print_endline text ;
      Lwt_io.(with_file ~mode:output path (fun oc -> write oc text))
    in
    Lwt_list.iter_p f file_dumps

  let perform ~stdout ~stderr (Sh cmd) =
    let script_file = Filename.temp_file "guizmin" ".sh" in
    Lwt_io.(with_file
              ~mode:output script_file
              (fun oc -> write oc cmd)) >>= fun () ->
    redirection stdout >>= fun stdout ->
    redirection stderr >>= fun stderr ->
    print_endline cmd ;
    let cmd = "", [| "sh" ; script_file |] in
    Lwt_process.exec ~stdout ~stderr cmd >>= fun status ->
    Lwt_unix.unlink script_file >>| fun () ->
    Caml.Unix.(match status with
        | WEXITED code
        | WSIGNALED code
        | WSTOPPED code -> code
      )
end

let perform_step (Allocator.Resource { np ; mem }) ({ db } as config) ({ cmd ; np ; mem } as task) =
  let uid = Unix.getuid () in
  let env = make_execution_env config ~np ~mem task in
  let stdout = Db.stdout db task.id in
  let stderr = Db.stderr db task.id in
  remove_if_exists env.tmp_dir >>= fun () ->
  Unix.mkdir_p env.tmp ;

  Concrete_task.generate_file_dumps env cmd >>= fun () ->
  Concrete_task.(perform ~stdout ~stderr (of_cmd env cmd)) >>= fun exit_status ->

  if config.use_docker then ( (* FIXME: not necessary if no docker command was run *)
    sprintf "docker run -v %s:/bistro -t busybox chown -R %d /bistro" env.tmp_dir uid
    |> Sys.command
    |> ignore
  ) ;
  let dest_exists = Sys.file_exists env.dest = `Yes in
  (
    if exit_status = 0 && dest_exists then
      mv env.dest (Db.cache config.db task.id) >>= fun () ->
      remove_if_exists env.tmp_dir
    else
      Lwt.return ()
  ) >>= fun () ->
  Lwt.return (
    if exit_status = 0 then
      Ok ()
    else
      R.error_msgf "Failed with code %d" exit_status
  )

let perform_input p =
  Lwt.wrap (fun () ->
      if Sys.file_exists p <> `Yes then
        R.error_msgf
          "File %s is declared as an input of a workflow but does not exist."
          p
      else
        Ok ()
    )

let select_dir_path db = function
  | `Input p -> string_of_path p
  | `Step id -> Db.cache db id

let select_path db dir q =
  let p = select_dir_path db dir in
  let q = string_of_path q in
  Filename.concat p q

let perform_select db dir q =
  Lwt.wrap (fun () ->
      let path = select_path db dir q in
      if Sys.file_exists path <> `Yes then (
        R.error_msgf
          "No file or directory named %s in directory workflow %s"
          (string_of_path q) (select_dir_path db dir)
      )
      else Ok ()
    )

let perform alloc config = function
  | Input (_, p) -> perform_input (string_of_path p)
  | Select (_, dir, q) -> perform_select config.db dir q
  | Step s -> perform_step alloc config s

let is_done { db } t =
  let path = match t with
    | Input (_, p) -> string_of_path p
    | Select (_, dir, q) -> select_path db dir q
    | Step { id } -> Db.cache db id
  in
  Lwt.return (Sys.file_exists path = `Yes)

let clean { db } = function
  | Input _ | Select _ -> Lwt.return ()
  | Step s ->
    remove_if_exists (Db.cache db s.id) >>= fun () ->
    remove_if_exists (Db.stdout db s.id) >>= fun () ->
    remove_if_exists (Db.stderr db s.id)
