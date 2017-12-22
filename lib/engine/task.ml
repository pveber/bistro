open Core

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let mv src dst =
  Lwt_process.exec ("", [| "mv" ; src ; dst |]) >>| ignore

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-rf" ; fn |]) >>| ignore
  else
    Lwt.return ()

let touch dst =
  Lwt_process.exec ("", [| "touch" ; dst |]) >>| ignore

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

include Bistro.U

type result =
  | Input_check of { path : string ; pass : bool }
  | Select_check of { dir_path : string ; sel : string list ; pass : bool }
  | Step_result of {
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      step : Bistro.step ;
      exit_code : int ;
      action : [`Sh of string | `Eval] ;
      dumps : (string * string) list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }

type config = {
  db : Db.t ;
  use_docker : bool ;
  keep_all : bool ;
  precious : String.Set.t ;
}

let config ~db_path ~use_docker ~keep_all ~precious = {
  db = Db.init_exn db_path ;
  use_docker ;
  keep_all ;
  precious ;
}

let requirement =
  let open Bistro in
  function
  | Input _
  | Select _ -> Allocator.Request { np = 0 ; mem = 0 }
  | Step { np ; mem ; _ } ->
    Allocator.Request { np ; mem }

let rec command_uses_docker =
  let open Bistro.Command in
  function
  | Docker (_, _) -> true
  | Simple_command _ -> false
  | And_list xs
  | Or_list xs
  | Pipe_list xs -> List.exists xs ~f:command_uses_docker

let action_uses_docker =
  let open Bistro in
  function
  | Exec cmd -> command_uses_docker cmd
  | Eval _ -> false

type execution_env = {
  use_docker : bool ;
  tmp_dir : string ;
  dest : string ;
  tmp : string ;
  dep : Bistro.dep -> string ;
  file_dump : Bistro.dep Bistro.Command.token list -> string ;
  np : int ;
  mem : int ;
}

let make_execution_env { db ; use_docker ; _ } ~np ~mem step =
  let tmp_dir = Db.tmp db step.Bistro.id in
  let path_of_task_id tid = Db.cache db tid in
  let dep = function
    | `Input p ->
      let p = Bistro.Path.to_string p in
      if Filename.is_relative p then
        Filename.concat (Sys.getcwd ()) p
      else
        p
    | `Task tid -> path_of_task_id tid
    | `Select (tid, p) ->
      Filename.concat (path_of_task_id tid) (Bistro.Path.to_string p)
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
  type t =
    | Sh of string
    | Eval of (unit -> unit)

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
    let open Bistro.Command in
    function
    | NP
    | DEST
    | TMP
    | S _
    | D _
    | MEM
    | EXE -> []
    | F f ->
      (`File_dump (f, in_docker)) :: file_dumps_of_tokens in_docker f
      |> List.dedup

  let rec file_dumps_of_command in_docker =
    let open Bistro.Command in
    function
    | Simple_command toks -> file_dumps_of_tokens in_docker toks
    | And_list xs
    | Or_list xs
    | Pipe_list xs ->
      List.map xs ~f:(file_dumps_of_command in_docker)
      |> List.concat
      |> List.dedup
    | Docker (_, cmd) -> file_dumps_of_command true cmd

  let file_dumps_of_action =
    let open Bistro in
    function
    | Exec cmd -> file_dumps_of_command false cmd
    | Eval _ -> []

  let token env =
    let open Bistro.Command in
    function
    | S s -> s
    | D d -> env.dep d
    | F toks -> env.file_dump toks
    | DEST -> env.dest
    | TMP -> env.tmp
    | NP -> string_of_int env.np
    | MEM -> string_of_int env.mem
    | EXE -> Sys.argv.(0)

  let string_of_tokens env xs =
    List.map ~f:(token env) xs
    |> String.concat

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
    let open Bistro.Command in
    function
    | Simple_command tokens -> string_of_tokens env tokens
    | And_list xs -> par (string_of_command_aux env " && " xs)
    | Or_list xs -> par (string_of_command_aux env " || " xs)
    | Pipe_list xs -> par (string_of_command_aux env " | " xs)
    | Docker (image, cmd) ->
      if env.use_docker then
        let dck_env = make_docker_execution_env env in
        sprintf
          "docker run --log-driver=none --rm %s %s %s %s -i %s bash -c '%s'"
          (deps_mount env dck_env (deps cmd))
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

  let of_action env =
    let open Bistro in
    function
    | Exec cmd -> of_cmd env cmd
    | Eval { f ; _ } ->
      let env = object
        method dep = env.dep
        method np = env.np
        method mem = env.mem
        method tmp = env.tmp
        method dest = env.dest
      end
      in
      Eval (fun () -> f env)

  let extract_file_dumps env action =
    let file_dumps = file_dumps_of_action action in
    let f (`File_dump (toks, in_docker)) =
      let exec_env = if in_docker && env.use_docker then make_docker_execution_env env else env in
      let path = env.file_dump toks in
      let text = string_of_tokens exec_env toks in
      path, text
    in
    `File_dumps (List.map file_dumps ~f)

  let write_file_dumps (`File_dumps xs) =
    let f (path, text) =
      Lwt_io.(with_file ~mode:output path (fun oc -> write oc text))
    in
    Lwt_list.iter_p f xs

  let perform_command ~stdout ~stderr cmd =
    let script_file = Filename.temp_file "guizmin" ".sh" in
    Lwt_io.(with_file
              ~mode:output script_file
              (fun oc -> write oc cmd)) >>= fun () ->
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

  let rec waitpid pid =
    try Unix.waitpid pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid

  let perform_eval ~stdout ~stderr f =
    touch stdout >>= fun () ->
    touch stderr >>= fun () ->
    let (read_from_child, write_to_parent) = Unix.pipe () in
    let (read_from_parent, write_to_child) = Unix.pipe () in
    match Unix.fork () with
    | `In_the_child ->
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      let ecode =
        try f () ; 0
        with e ->
          Out_channel.with_file stderr ~f:(fun oc ->
              fprintf oc "%s\n" (Exn.to_string e) ;
              Printexc.print_backtrace oc
            ) ;
          1
      in
      let oc = Unix.out_channel_of_descr write_to_parent in
      Marshal.to_channel oc ecode [] ;
      Caml.flush oc ;
      Unix.close write_to_parent ;
      ignore (Caml.input_value (Unix.in_channel_of_descr read_from_parent)) ;
      assert false
    | `In_the_parent pid ->
      Unix.close write_to_parent ;
      Unix.close read_from_parent ;
      let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input read_from_child in
      Lwt_io.read_value ic >>= fun (ecode : int) ->
      Caml.Unix.kill (Pid.to_int pid) Caml.Sys.sigkill;
      ignore (waitpid pid) ;
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      Lwt.return ecode

  let perform ~stdout ~stderr = function
    | Sh cmd -> perform_command ~stdout ~stderr cmd
    | Eval f -> perform_eval ~stdout ~stderr f
end

let docker_chown dir uid =
  sprintf "docker run --log-driver=none --rm -v %s:/bistro -i busybox chown -R %d /bistro" dir uid
  |> Sys.command
  |> ignore

let perform_step
    (Allocator.Resource { np ; mem })
    ({ db ; _ } as config)
    ({ Bistro.action ; _ } as step) =
  let uid = Unix.getuid () in
  let env = make_execution_env config ~np ~mem step in
  let stdout = Db.stdout db step.id in
  let stderr = Db.stderr db step.id in
  remove_if_exists env.tmp_dir >>= fun () ->
  Unix.mkdir_p env.tmp ;

  let ct = Concrete_task.of_action env action in
  let file_dumps = Concrete_task.extract_file_dumps env action in
  Concrete_task.write_file_dumps file_dumps >>= fun () ->
  Concrete_task.(perform ~stdout ~stderr ct) >>= fun exit_code ->

  let dest_exists = Sys.file_exists env.dest = `Yes in
  let cache_dest = Db.cache config.db step.id in
  let success = exit_code = 0 && dest_exists in
  if config.use_docker && action_uses_docker action then (
    docker_chown env.tmp_dir uid ;
    if dest_exists then docker_chown env.dest uid
  ) ;
  (
    if success then
      mv env.dest cache_dest >>= fun () ->
      remove_if_exists env.tmp_dir
    else
      Lwt.return ()
  ) >>= fun () ->
  let action = match ct with
      Concrete_task.Sh cmd -> `Sh cmd
    | Concrete_task.Eval _ -> `Eval
  and `File_dumps dumps = file_dumps
  and outcome = match exit_code = 0, dest_exists with
      true, true -> `Succeeded
    | false, _ -> `Failed
    | true, false -> `Missing_output
  in
  Lwt.return (
    Step_result {
      outcome ;
      step ;
      exit_code ;
      action ;
      dumps ;
      cache = if success then Some cache_dest else None ;
      stdout ;
      stderr ;
    }
  )

let perform_input path =
  Lwt.wrap (fun () ->
      let pass = Sys.file_exists path = `Yes in
      Input_check { path ; pass }
    )

let select_dir_path db = function
  | `Input p -> Bistro.Path.to_string p
  | `Step id -> Db.cache db id

let select_path db dir q =
  let p = select_dir_path db dir in
  let q = Bistro.Path.to_string q in
  Filename.concat p q

let perform_select db dir sel =
  Lwt.wrap (fun () ->
      let p = select_path db dir sel in
      let pass = Sys.file_exists p = `Yes in
      Select_check {
        dir_path = select_dir_path db dir ;
        sel ;
        pass ;
      }
    )

let dep_of_select_child =
  let open Bistro in
  function
  | Input (_, p) -> `Input p
  | Step { id ; _ } -> `Step id
  | Select _ -> assert false

let perform alloc config =
  let open Bistro in
  function
  | Input (_, p) -> perform_input (Bistro.Path.to_string p)
  | Select (_, dir, q) -> perform_select config.db (dep_of_select_child dir) q
  | Step s -> perform_step alloc config s

let is_done t { db ; _ } =
  let open Bistro in
  let path = match t with
    | Input (_, p) -> Bistro.Path.to_string p
    | Select (_, dir, q) -> select_path db (dep_of_select_child dir) q
    | Step { id ; _ } ->
      let b = Db.cache db id in
      (*      printf "%s %s\n" descr b ; *) b
  in
  Lwt.return (Sys.file_exists path = `Yes)

let clean t { db ; _ } =
  let open Bistro in
  match t with
  | Input _ | Select _ -> Lwt.return ()
  | Step s ->
    remove_if_exists (Db.cache db s.id) >>= fun () ->
    remove_if_exists (Db.stdout db s.id) >>= fun () ->
    remove_if_exists (Db.stderr db s.id)

let post_revdeps_hook t config ~all_revdeps_succeeded =
  let open Bistro in
  match t with
  | Input _ | Select _ -> Lwt.return ()
  | Step s ->
    if
      not config.keep_all
      && not (String.Set.mem config.precious s.id)
      && all_revdeps_succeeded
    then clean t config
    else Lwt.return ()

let failure = function
  | Input_check { pass ; _ }
  | Select_check { pass ; _ } -> not pass
  | Step_result { outcome = `Succeeded ; _ } -> false
  | Step_result { outcome = (`Failed | `Missing_output) ; _ } -> true

let render_step_command ~np ~mem config task cmd =
  let open Concrete_task in
  let env = make_execution_env ~np ~mem config task in
  match of_cmd env cmd with
  | Sh cmd -> cmd
  | Eval _ -> ""

let render_step_dumps ~np ~mem config s =
  let env = make_execution_env ~np ~mem config s in
  let `File_dumps res = Concrete_task.extract_file_dumps env s.action in
  res
