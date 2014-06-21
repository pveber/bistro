open Core.Std

type backend =
  np:int -> mem:int -> timeout:Bistro_workflow.duration ->
  interpreter:Bistro_workflow.interpreter ->
  stdout:string -> stderr:string ->
  string -> [`Ok | `Error] Lwt.t


let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let cmd_of_interpreter script_file interpreter =
  let cmd = match interpreter with
    | `bash -> [| "bash" ; script_file |]
    | `ocaml -> [| "ocaml" ; script_file |]
    | `perl -> [| "perl" ; script_file |]
    | `python -> [| "python" ; script_file |]
    | `R -> [| "R" ; "CMD" ; script_file |]
    | `sh -> [| "sh" ; script_file |]
  in
  ("", cmd)

let string_of_process_status ps =
  Unix.Exit_or_signal.(
    of_unix ps
    |> to_string_hum
  )

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let local_worker_aux (log : Bistro_log.t) ~np ~mem ~timeout ~interpreter ~stdout ~stderr script =
  let ext = Bistro_workflow.extension_of_interpreter interpreter in
  let script_file = Filename.temp_file "bistro" ("." ^ ext) in
  Bistro_log.debug log "Exec script %s:\n%s\n" script_file script ;
  Lwt_io.(with_file ~mode:output script_file (fun oc -> write oc script)) >>= fun () ->
  begin
    redirection stdout >>= fun stdout ->
    redirection stderr >>= fun stderr ->
    let cmd = cmd_of_interpreter script_file interpreter in
    Lwt_process.exec ~stdout ~stderr cmd
  end >>= fun exitcode ->
  match exitcode with
  | Caml.Unix.WEXITED 0 ->
    Lwt_unix.unlink script_file >>= fun () ->
    Lwt.return `Ok
  | _ -> (
    Bistro_log.error log
      "Script %s failed!\nError status: %s\nstdout: %s\nstderr: %s\n"
      script_file (string_of_process_status exitcode) stdout stderr ;
    Lwt.return `Error
  )

let local_worker ~np ~mem log : backend =
  let pool = Bistro_pool.create ~np ~mem in
  fun ~np ~mem ~timeout ~interpreter ~stdout ~stderr script ->
    Bistro_pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
      local_worker_aux log ~np ~mem ~stdout ~stderr ~interpreter ~timeout script
    )


let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-r" ; fn |]) >|= ignore
  else
    Lwt.return ()

let thread_of_rule blog (backend : backend) db r dep_threads =
  let open Bistro_workflow in
  match r with { np ; mem ; timeout ; interpreter ; script } ->
  let x = Rule r in
  Lwt.join dep_threads >>= fun () ->
  (
    let stdout = Bistro_db.stdout_path db x in
    let stderr = Bistro_db.stderr_path db x in
    let dest = Bistro_db.build_path db x in
    let tmp = Bistro_db.tmp_path db x in
    let script = script_to_string ~dest ~tmp (Bistro_db.path db) script in
    remove_if_exists stdout >>= fun () ->
    remove_if_exists stderr >>= fun () ->
    remove_if_exists dest >>= fun () ->
    remove_if_exists tmp >>= fun () ->
    Lwt_unix.mkdir tmp 0o750 >>= fun () ->
    Bistro_log.started_build blog x ;
    backend ~np ~mem ~timeout ~interpreter ~stdout ~stderr script >>= fun backend_response ->
    match backend_response, Sys.file_exists_exn dest with
    | `Ok, true ->
      Bistro_log.finished_build blog x ;
      remove_if_exists tmp >>= fun () ->
      Lwt_unix.rename dest (Bistro_db.path db x)
    | `Ok, false ->
      let msg = "rule failed to produce its target at the prescribed location" in
      Bistro_log.failed_build ~msg blog x ;
      Lwt.fail (Failure msg)
    | `Error, _ ->
      Bistro_log.failed_build blog x ;
      Lwt.fail (Failure (sprintf "Build of workflow %s failed!" (Bistro_workflow.digest x)))
  )


let thread_of_rule_fake db r dep_threads =
  let open Bistro_workflow in
  let x = Rule r in
  Lwt.join dep_threads >>= fun () -> (
    let output = Bistro_db.path db x in
    Lwt_io.printf
      "Rule(%s): exec script\n\t%s\n"
      output
      (script_to_string ~dest:output ~tmp:(Bistro_db.tmp_path db x) (Bistro_db.path db) r.script)
  )

let rec thread_of_workflow f db map w =
  let open Bistro_workflow in
  let id = digest w in
  match Map.find map id with
  | Some t -> (t, map)
  | None -> (
      let open Bistro_workflow in
      match w with
      | Input p ->
        Lwt.wrap (fun () ->
            if Sys.file_exists p <> `Yes
            then failwithf "File %s is declared as an input of a workflow but does not exist." p ()
          ),
        map
      | Select (dir,p) as x ->
        let dir_path = Bistro_db.path db dir in
        let check_in_dir () =
          if Sys.file_exists (Bistro_db.path db x) <> `Yes
          then (
            let msg = sprintf "No file or directory named %s in directory workflow." p in
            Lwt.fail (Failure msg)
          )
          else Lwt.return ()
        in
        if Sys.file_exists_exn dir_path then check_in_dir (), map
        else (
          let dir_thread, map = thread_of_workflow f db map dir in
          dir_thread >>= check_in_dir,
          map
        )
      | Rule r as x ->
        let dest_path = Bistro_db.path db x in
        if Sys.file_exists_exn dest_path then Lwt.return (), map
        else (
          let dep_threads, map =
            List.fold_right (Bistro_workflow.deps w) ~init:([], map) ~f:(fun w (accu, map) ->
                let t, map = thread_of_workflow f db map w in
                t :: accu, map
              )
          in
          let t = f db r dep_threads in
          t, String.Map.add map ~key:id ~data:t
        )
    )

let run db blog backend w =
  fst (thread_of_workflow (thread_of_rule blog backend) db String.Map.empty (Bistro_workflow.u w))

let dryrun db  w =
  fst (thread_of_workflow thread_of_rule_fake db String.Map.empty (Bistro_workflow.u w))

let build_repo ~base ?wipeout db blog backend ((Bistro_repo.Repo items) as repo) =
  List.fold_left items ~init:([], String.Map.empty) ~f:(fun (roots,accu) (Bistro_repo.Item (u,_,_)) ->
      let t, map = thread_of_workflow (thread_of_rule blog backend) db accu u in
      t :: roots, map
    )
  |> fst
  |> Lwt.join
  >>= fun () ->
  Bistro_repo.setup ?wipeout db repo base ;
  Lwt.return ()

module Daemon = struct
  (* FIXME: this implementation leaks memory. When calling run on a
     [d] with new workflows, the size of [d.threads] grows without any
     possibility to shrink. A good fix should be to switch to a weak
     array to store threads. *)
  type t = {
    mutable threads : unit Lwt.t String.Map.t ;
    mutable on : bool ;
    db : Bistro_db.t ;
    log : Bistro_log.t ;
    backend : backend ;
  }

  let make db log backend = {
    threads = String.Map.empty ;
    on = true ;
    db ; log ; backend
  }

  let send' d u =
    if d.on then (
      let t, threads = thread_of_workflow (thread_of_rule d.log d.backend) d.db String.Map.empty u in
      d.threads <- threads ;
      Some t
    )
    else None

  let send d w = send' d (Bistro_workflow.u w)

  let shutdown d =
    d.on <- false ;
    String.Map.to_alist d.threads
    |> List.map ~f:snd
    |> Lwt.join
end
