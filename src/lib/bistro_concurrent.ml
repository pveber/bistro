open Core.Std

let ( >>= ) = Lwt.( >>= )

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

let thread_of_workflow_exec db w dep_threads =
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
  | Rule r as x ->
    Lwt.join dep_threads >>= fun () -> (
      Bistro_db.with_logger db x ~f:(fun log ->
	Out_channel.with_file (Bistro_db.stdout_path db x) ~f:(fun stdout ->
	  Out_channel.with_file (Bistro_db.stderr_path db x) ~f:(fun stderr ->
	    let tmp_path = Bistro_db.tmp_path db x in
	    let f cmd =
	      let tokens = exec_cmd tmp_path (Bistro_db.path db) cmd in
	      let line = String.concat ~sep:" " tokens in
	      shell ~stdout ~stderr log line
	    in
	    Lwt_list.iter_s f r.cmds >>= fun () ->
	    Unix.rename ~src:tmp_path ~dst:(Bistro_db.cache_path db x) ;
	    Lwt.return ()
	  )
	)
      )
    )


let thread_of_workflow_fake_exec db w dep_threads =
  let open Bistro_workflow in
  match w with
  | Input p ->
    Lwt_io.printf "Input(%s): check that file exists\n" p ;
  | Select (dir,p) as x ->
    Lwt_io.printf "Dir(%s,%s): check that file exists\n" (Bistro_db.path db dir) p ;
  | Rule r as x ->
    Lwt.join dep_threads >>= fun () -> (
      let output = Bistro_db.path db x in
      let f cmd =
	let tokens = exec_cmd output (Bistro_db.path db) cmd in
	String.concat ~sep:" " tokens
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

let exec db w =
  fst (thread_of_workflow thread_of_workflow_exec db String.Map.empty (w : _ Bistro_workflow.t :> Bistro_workflow.u))

let dryrun db  w =
  fst (thread_of_workflow thread_of_workflow_fake_exec db String.Map.empty (w : _ Bistro_workflow.t :> Bistro_workflow.u))
