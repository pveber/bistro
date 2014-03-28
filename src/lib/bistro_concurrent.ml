open Core.Std

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let shell
    ~logger
    ~stdout
    ~stderr
    script =
  let script_file = Filename.temp_file "bistro" ".sh" in
  Bistro_logger.info logger "Started script:\n\n%s\n\n" script ;
  Lwt_io.(with_file ~mode:output script_file (fun oc -> write oc script)) >>= fun () ->
  Lwt_process.exec ~stdout ~stderr
    ("", [| "sh" ; script_file |]) >>= fun exitcode ->
  match exitcode with
  | Caml.Unix.WEXITED 0 -> Lwt.return ()
  | _ -> (
    let msg = Printf.sprintf "shell script %s failed" script_file in
    Bistro_logger.error logger "%s" msg ;
    Lwt.fail (Failure msg)
  )

type backend =
  np:int -> mem:int ->
  stdout:string -> stderr:string ->
  Bistro_logger.t ->
  string -> unit Lwt.t

let string_of_cmd (h, t) =
  String.concat ~sep:" " (h :: (Array.to_list t))

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let make_cmd cmds =
  String.concat ~sep:" && \\\n " cmds
  |> Lwt_process.shell

let local_worker ~np ~mem : backend =
  let pool = Bistro_pool.create ~np ~mem in
  fun ~np ~mem ~stdout ~stderr logger script ->
    Bistro_pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
      redirection stdout >>= fun stdout ->
      redirection stderr >>= fun stderr ->
      shell ~logger ~stdout ~stderr script
    )

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
     Lwt.join dep_threads >>= fun () ->
     if Sys.file_exists (Bistro_db.path db x) <> `Yes
     then (
       let msg = sprintf "No file or directory named %s in directory workflow." p in
       Lwt.fail (Failure msg)
     )
     else Lwt.return ()
  | Rule { np ; mem ; script } as x ->
    let dest_path = Bistro_db.cache_path db x in
    Lwt.return () >>= fun () ->
    if Sys.file_exists_exn dest_path then Lwt.return ()
    else (
      Lwt.join dep_threads >>= fun () -> (
	let stdout_path = Bistro_db.stdout_path db x in
	let stderr_path = Bistro_db.stderr_path db x in
	let build_path = Bistro_db.build_path db x in
	let tmp_path = Bistro_db.tmp_path db x in
	let script = Script.to_string ~dest:build_path ~tmp:tmp_path (Bistro_db.path db) script in
	remove_if_exists stdout_path >>= fun () ->
	remove_if_exists stderr_path >>= fun () ->
	remove_if_exists build_path >>= fun () ->
	remove_if_exists tmp_path >>= fun () ->
	Lwt_unix.mkdir tmp_path 0o750 >>= fun () ->
	Bistro_logger.started blog x ;
	backend ~np:np ~mem:mem ~stdout:stdout_path ~stderr:stderr_path blog script >>= fun () ->
	if Sys.file_exists build_path = `Yes then (
	  Bistro_logger.finished blog x ;
	  remove_if_exists tmp_path >>= fun () ->
	  Lwt_unix.rename build_path dest_path
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
      Lwt_io.printf
	"Rule(%s): exec script\n\t%s\n"
	output
	(Script.to_string ~dest:output ~tmp:(Bistro_db.tmp_path db x) (Bistro_db.path db) r.script)
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

let build_repo ~base ?wipeout db blog backend ((Bistro_repo.Repo items) as repo) =
  List.fold_left items ~init:([], String.Map.empty) ~f:(fun (roots,accu) (Bistro_repo.Item (u,_,_)) ->
    let t, map = thread_of_workflow (thread_of_workflow_exec blog backend) db accu u in
    t :: roots, map
  )
  |> fst
  |> Lwt.join
  >>= fun () ->
  Bistro_repo.setup ?wipeout db repo base ;
  Lwt.return ()

