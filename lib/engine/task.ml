open Core

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let mv src dst =
  Lwt_process.exec ("", [| "mv" ; src ; dst |]) >>| ignore

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-rf" ; fn |]) >>| ignore
  else
    Lwt.return ()

let docker_chown dir uid =
  sprintf "docker run --log-driver=none --rm -v %s:/bistro -i busybox chown -R %d /bistro" dir uid
  |> Sys.command
  |> ignore

let touch dst =
  Lwt_process.exec ("", [| "touch" ; dst |]) >>| ignore

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

include Bistro.U

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type result =
  | Input_check of { path : string ; pass : bool }
  | Select_check of { dir_path : string ; sel : string list ; pass : bool }
  | Step_result of {
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      step : Bistro.step ;
      exit_code : int ;
      action : [`Sh of string | `Eval] ;
      file_dumps : file_dump list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }
  | Map_command_result of {
      pass : bool ;
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

type execution_env = {
  using_docker : bool ;
  tmp_dir : string ; (* host all execution *)
  dest : string ;    (* expected path for the target *)
  tmp : string ;     (* temp dir for the process *)
  stdout : string ;
  stderr : string ;
  dep : Bistro.dep -> string ;
  file_dump : Bistro.dep Bistro.Command.token list -> string ;
  np : int ;
  mem : int ;
  uid : int ;
}

let make_execution_env { db ; use_docker ; _ } ~np ~mem id =
  let tmp_dir = Db.tmp db id in
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
    | `Map _ -> assert false
  in
  let file_dump toks =
    Filename.concat tmp_dir (digest toks)
  in
  {
    tmp_dir ;
    using_docker = use_docker ;
    tmp = Filename.concat tmp_dir "tmp" ;
    dest = Filename.concat tmp_dir "dest" ;
    stdout = Db.stdout db id ;
    stderr = Db.stderr db id ;
    file_dump ;
    dep ;
    np ;
    mem ;
    uid = Unix.getuid () ;
  }

let make_docker_execution_env env = {
  tmp_dir = "/bistro" ;
  using_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = (fun d -> sprintf "/bistro/data/%s" (digest d)) ;
  file_dump = (fun toks -> sprintf "/bistro/data/%s" (digest toks)) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}

module Step_task :
sig
  val perform :
    Allocator.resource ->
    config ->
    Bistro.step -> result Lwt.t
end
=
struct
  type symbolic_file_dump = Symbolic_file_dump of {
    contents : Bistro.dep Bistro.Command.token list ;
    in_docker : bool ;
  }
  type cmd = Command of {
    text : string ;
    file_dumps : file_dump list ;
    env : execution_env ;
    uses_docker : bool ;
  }
  type t =
    | Sh of { env : execution_env ; cmd : cmd }
    | Par_sh of {
        cmds : cmd list ;
        dir_contents : string list ;
        env : execution_env ;
      }
    | Eval of {
        f : unit -> unit ;
        env : execution_env ;
      }

  (* let render_commands = function
   *   | Sh { cmd = Command c ; _ } -> [ c.text ]
   *   | Par_sh { cmds ; _ } -> List.map cmds ~f:(fun (Command cmd) -> cmd.text)
   *   | Eval _ -> []
   * 
   * let render_file_dumps = function
   *   | Sh { cmd = Command c ; _ } -> c.file_dumps
   *   | Par_sh { cmds ; _ } ->
   *     List.concat_map cmds ~f:(fun (Command cmd) ->
   *         cmd.file_dumps
   *       )
   *   | Eval _ -> [] *)

  let docker_image_url image =
    sprintf "%s%s/%s%s"
      (Option.value_map ~default:"" ~f:(sprintf "%s/") image.Bistro.dck_registry)
      image.Bistro.dck_account
      image.Bistro.dck_name
      (Option.value_map ~default:"" ~f:(sprintf ":%s")  image.Bistro.dck_tag)

  let rec file_dumps_of_tokens in_docker toks =
    List.map toks ~f:(file_dumps_of_token in_docker)
    |> List.concat
    |> List.dedup_and_sort

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
    | F contents ->
      Symbolic_file_dump {
        contents = contents ;
        in_docker = in_docker
      } :: file_dumps_of_tokens in_docker contents
      |> List.dedup_and_sort

  let rec file_dumps_of_command in_docker =
    let open Bistro.Command in
    function
    | Simple_command toks -> file_dumps_of_tokens in_docker toks
    | And_list xs
    | Or_list xs
    | Pipe_list xs ->
      List.map xs ~f:(file_dumps_of_command in_docker)
      |> List.concat
      |> List.dedup_and_sort
    | Docker (_, cmd) -> file_dumps_of_command true cmd

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
    let f (Symbolic_file_dump { contents = fd ; _ }) =
      sprintf "-v %s:%s" (env.file_dump fd) (dck_env.file_dump fd)
    in
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
      if env.using_docker then
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

  let compile_file_dump env (Symbolic_file_dump { contents ; in_docker }) =
    let exec_env =
      if in_docker && env.using_docker
      then make_docker_execution_env env
      else env
    in
    let path = env.file_dump contents in
    let text = string_of_tokens exec_env contents in
    File_dump { path ; text }

  let make_cmd env cmd =
    Command {
      text = string_of_command env cmd ;
      file_dumps =
        file_dumps_of_command false cmd
        |> List.map ~f:(compile_file_dump env) ;
      env ;
      uses_docker = command_uses_docker cmd ;
    }

  let make env { Bistro.action ; _ } = match action with
    | Exec cmd ->
      Sh {
        env ;
        cmd = make_cmd env cmd
      }
    | Par_exec { dir ; cmd } ->
      let dir_contents =
        env.dep (Bistro.U.to_dep dir)
        |> Sys.readdir
        |> Array.to_list
      in
      let cmds = List.map dir_contents ~f:(fun fn ->
          let chunk_dir = sprintf "%s/%s/%s" env.tmp_dir "chunks" fn in
          let chunk_env = {
            env with
            tmp_dir = chunk_dir ;
            dest = Filename.concat chunk_dir "dest" ;
            tmp = Filename.concat chunk_dir "tmp" ;
            stdout = Filename.concat chunk_dir "stdout" ;
            stderr = Filename.concat chunk_dir "stderr" ;
            np = 1 (* FIXME *) ;
            mem = 1 (* FIXME *);
          } in
          Bistro.U.select dir [fn]
          |> cmd
          |> Bistro.Command.map ~f:Bistro.U.to_dep
          |> make_cmd chunk_env
        )
      in
      Par_sh {
        cmds ;
        dir_contents = List.map dir_contents ~f:(fun fn ->
            Filename.concat env.dest fn
          ) ;
        env ;
      }
    | Eval { f ; _ } ->
      let obj_env = object
        method dep = env.dep
        method np = env.np
        method mem = env.mem
        method tmp = env.tmp
        method dest = env.dest
      end
      in
      Eval {
        f = (fun () -> f obj_env) ;
        env ;
      }

  let write_file_dumps xs =
    let f (File_dump { text ; path }) =
      Lwt_io.(with_file ~mode:output path (fun oc -> write oc text))
    in
    Lwt_list.iter_p f xs

  let outcome ~exit_code ~dest_exists=
    match exit_code = 0, dest_exists with
      true, true -> `Succeeded
    | false, _ -> `Failed
    | true, false -> `Missing_output

  let run_command (Command cmd) =
    let script_file = Filename.temp_file "guizmin" ".sh" in
    remove_if_exists cmd.env.tmp_dir >>= fun () ->
    Unix.mkdir_p cmd.env.tmp ;
    write_file_dumps cmd.file_dumps >>= fun () ->
    Lwt_io.(with_file
              ~mode:output script_file
              (fun oc -> write oc cmd.text)) >>= fun () ->
    redirection cmd.env.stdout >>= fun stdout ->
    redirection cmd.env.stderr >>= fun stderr ->
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
    if cmd.env.using_docker && cmd.uses_docker then (
      docker_chown cmd.env.tmp_dir cmd.env.uid ;
      if dest_exists then docker_chown cmd.env.dest cmd.env.uid
    ) ;
    Lwt.return (exit_code, outcome ~dest_exists ~exit_code)


  let perform_par_command ~env ~cmds ~dir_contents =
    let f (dest, (Command cmd as command)) =
      if Sys.file_exists dest = `Yes then Lwt.return `Succeeded
      else
        run_command command >>= fun (_, outcome) ->
        (
          if outcome = `Succeeded then
            mv cmd.env.dest dest >>= fun () ->
            remove_if_exists cmd.env.tmp_dir
          else
            Lwt.return ()
        ) >>= fun () ->
        Lwt.return outcome
  in
  Unix.mkdir_p env.dest ;
  List.map2_exn dir_contents cmds ~f:(fun dst cmd -> dst, cmd)
  |> Lwt_list.map_s f >>= fun results ->
  let success = List.for_all results ~f:(( = ) `Succeeded) in
  (* FIXME copy stdout/stderr *)
  Lwt.return (
    (if success then `Succeeded else `Failed (*FIXME*)),
    (if success then 0 else 1 (*FIXME*)),
    `Sh "", (* FIXME *)
    [] (* FIXME *)
  )

  let rec waitpid pid =
    try Unix.waitpid pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid

  let perform_eval step cache_dest env f =
    touch env.stdout >>= fun () ->
    touch env.stderr >>= fun () ->
    let (read_from_child, write_to_parent) = Unix.pipe () in
    let (read_from_parent, write_to_child) = Unix.pipe () in
    match Unix.fork () with
    | `In_the_child ->
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      let exit_code =
        try f () ; 0
        with e ->
          Out_channel.with_file env.stderr ~f:(fun oc ->
              fprintf oc "%s\n" (Exn.to_string e) ;
              Printexc.print_backtrace oc
            ) ;
          1
      in
      let oc = Unix.out_channel_of_descr write_to_parent in
      Marshal.to_channel oc exit_code [] ;
      Caml.flush oc ;
      Unix.close write_to_parent ;
      ignore (Caml.input_value (Unix.in_channel_of_descr read_from_parent)) ;
      assert false
    | `In_the_parent pid ->
      Unix.close write_to_parent ;
      Unix.close read_from_parent ;
      let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input read_from_child in
      Lwt_io.read_value ic >>= fun (exit_code : int) ->
      Caml.Unix.kill (Pid.to_int pid) Caml.Sys.sigkill;
      ignore (waitpid pid) ;
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      let dest_exists = Sys.file_exists env.dest = `Yes in
      let outcome = outcome ~dest_exists ~exit_code in
      Lwt.return (
        outcome, exit_code, `Eval, []
      )

  let perform
      (Allocator.Resource { np ; mem })
      config
      step =
    let open Bistro in
    let env = make_execution_env config ~np ~mem step.id in
    let cache_dest = Db.cache config.db step.id in
    (
      match make env step with
      | Sh { cmd = Command c as cmd ; _ } ->
        run_command cmd >>= fun (exit_code, outcome) ->
        Lwt.return (outcome, exit_code, `Sh c.text, c.file_dumps)
      | Par_sh { cmds ; dir_contents ; env } ->
        perform_par_command ~env ~cmds ~dir_contents
      | Eval { f ; env } ->
        perform_eval step cache_dest env f
    ) >>= fun (outcome, exit_code, action, file_dumps) ->
    (
      if outcome = `Succeeded then
        mv env.dest cache_dest >>= fun () ->
        remove_if_exists env.tmp_dir
      else
        Lwt.return ()
    ) >>= fun () ->
    Lwt.return (
      Step_result {
        outcome ;
        step ;
        exit_code ;
        action ;
        file_dumps ;
        cache = if outcome = `Succeeded then Some cache_dest else None ;
        stdout = env.stdout ;
        stderr = env.stderr ;
      }
    )
end

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
  | Step step -> Step_task.perform alloc config step

let is_done t { db ; _ } =
  let open Bistro in
  let path = match t with
    | Input (_, p) -> Bistro.Path.to_string p
    | Select (_, dir, q) -> select_path db (dep_of_select_child dir) q
    | Step { id ; _ } -> Db.cache db id
  in
  Lwt.return (Sys.file_exists path = `Yes)

let clean t { db ; _ } =
  let open Bistro in
  match t with
  | Input _ | Select _ -> Lwt.return ()
  | Step { id ; _ } ->
    remove_if_exists (Db.cache db id) >>= fun () ->
    remove_if_exists (Db.stdout db id) >>= fun () ->
    remove_if_exists (Db.stderr db id)

let post_revdeps_hook t config ~all_revdeps_succeeded =
  let open Bistro in
  match t with
  | Input _ | Select _ -> Lwt.return ()
  | Step { id ; _ } ->
    if
      not config.keep_all
      && not (String.Set.mem config.precious id)
      && all_revdeps_succeeded
    then clean t config
    else Lwt.return ()

let failure = function
  | Map_command_result { pass ; _ }
  | Input_check { pass ; _ }
  | Select_check { pass ; _ } -> not pass
  | Step_result { outcome = `Succeeded ; _ } -> false
  | Step_result { outcome = (`Failed | `Missing_output) ; _ } -> true
