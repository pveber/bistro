open Core.Std

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let shell
    (log : _ Bistro_db.logger)
    ?(stdout = stdout)
    ?(stderr = stderr)
    s =
  log `debug "sh call:\n\n%s\n\n" s ;
  Lwt_process.exec
    ~stdout:(`FD_move (Unix.descr_of_out_channel stdout))
    ~stderr:(`FD_move (Unix.descr_of_out_channel stderr))
    (Lwt_process.shell s) >>= fun exitcode ->
  match exitcode with
  | Caml.Unix.WEXITED 0 -> Lwt.return ()
  | _ ->
    log `error "sh call exited with non-zero code:\n\n%s\n\n" s ;
    Lwt.fail (Failure (Printf.sprintf "shell call failed:\n%s\n" s))

type backend =
  np:int -> mem:int ->
  stdout:string -> stderr:string ->
  Bistro_logger.t ->
  Lwt_process.command list -> unit Lwt.t

let string_of_cmd (h, t) =
  String.concat ~sep:" " (h :: (Array.to_list t))

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let local_worker ~np ~mem : backend =
  let pool = Bistro_pool.create ~np ~mem in
  fun ~np ~mem ~stdout ~stderr logger cmds ->
    let exec cmd =
      redirection stdout >>= fun stdout ->
      redirection stderr >>= fun stderr ->
      Bistro_logger.info logger "Running\n\t%s\n" (string_of_cmd cmd) ;
      Lwt_process.exec ~stdout ~stderr cmd
      >>=
	function
	| Caml.Unix.WEXITED 0 -> Lwt.return ()
	| _ ->
	  let msg = Printf.sprintf "shell call failed:\n%s\n" (string_of_cmd cmd) in
	  Bistro_logger.error logger "%s" msg ;
	  Lwt.fail (Failure msg)
    in
    Bistro_pool.use pool ~np ~mem ~f:(fun ~np ~mem -> Lwt_list.iter_s exec cmds)

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-r" ; fn |]) >|= ignore
  else
    Lwt.return ()

let thread_of_workflow_exec blog (backend : backend) db w dep_threads =
  let open Bistro_workflow in
  match w with
  | Input p ->
    Lwt.wrap (fun () ->
      if Sys.file_exists p <> `Yes
      then failwithf "File %s is declared as an input of a workflow but does not exist." p ()
    )
  | Select (_,p) as x ->
    Lwt.wrap (fun () ->
      if Sys.file_exists (Bistro_db.path db x) <> `Yes
      then failwithf "No file or directory named %s in directory workflow." p ()
    )
  | Rule { np ; mem ; cmds } as x ->
    let dest_path = Bistro_db.cache_path db x in
    Lwt.return () >>= fun () ->
    if Sys.file_exists_exn dest_path then Lwt.return ()
    else (
      Lwt.join dep_threads >>= fun () -> (
	let stdout_path = Bistro_db.stdout_path db x in
	let stderr_path = Bistro_db.stderr_path db x in
	let build_path = Bistro_db.build_path db x in
	let tmp_path = Bistro_db.tmp_path db x in
	let f cmd =
	  let line = exec_cmd ~dest:build_path ~tmp:tmp_path (Bistro_db.path db) cmd in
	  Lwt_process.shell line
	in
	let cmds = List.map cmds ~f in
	remove_if_exists stdout_path >>= fun () ->
	remove_if_exists stderr_path >>= fun () ->
	remove_if_exists build_path >>= fun () ->
	remove_if_exists tmp_path >>= fun () ->
	Lwt_unix.mkdir tmp_path 0o640 >>= fun () ->
	Bistro_logger.started blog x ;
	backend ~np:np ~mem:mem ~stdout:stdout_path ~stderr:stderr_path blog cmds >>= fun () ->
	if Sys.file_exists tmp_path = `Yes then (
	  Bistro_logger.finished blog x ;
	  Lwt_unix.rename tmp_path dest_path
	)
	else
	  Lwt.fail (Failure "rule failed to produce its target at the prescribed location")
	)
    )


let thread_of_workflow_fake_exec db w dep_threads =
  let open Bistro_workflow in
  match w with
  | Input p ->
    Lwt_io.printf "Input(%s): check that file exists\n" p ;
  | Select (dir,p) ->
    Lwt_io.printf "Dir(%s,%s): check that file exists\n" (Bistro_db.path db dir) p ;
  | Rule r as x ->
    Lwt.join dep_threads >>= fun () -> (
      let output = Bistro_db.path db x in
      let f cmd =
	exec_cmd ~dest:output ~tmp:(Bistro_db.tmp_path db x) (Bistro_db.path db) cmd
      in
      Lwt_io.printf
	"Rule(%s): exec script\n\t%s\n"
	output
	(String.concat ~sep:"\n\t" (List.map r.cmds ~f))
    )

let rec thread_of_workflow f db map w =
  let open Bistro_workflow in
  let id = digest w in
  match Map.find map id with
  | Some t -> (t, map)
  | None -> (
    let dep_threads, map =
      List.fold_right (Bistro_workflow.deps w) ~init:([], map) ~f:(fun w (accu, map) ->
	let t, map = thread_of_workflow f db map w in
	t :: accu, map
      )
    in
    let t = f db w dep_threads in
    t, String.Map.add map ~key:id ~data:t
  )

let exec db blog backend w =
  fst (thread_of_workflow (thread_of_workflow_exec blog backend) db String.Map.empty (w : _ Bistro_workflow.t :> Bistro_workflow.u))

let dryrun db  w =
  fst (thread_of_workflow thread_of_workflow_fake_exec db String.Map.empty (w : _ Bistro_workflow.t :> Bistro_workflow.u))
