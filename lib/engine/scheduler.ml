open Core.Std
open Bistro
open Bistro.Workflow

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )
let ( >>=? ) x f = x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

module Pool : sig
  type t

  val create : np:int -> mem:int -> t
  val use : t -> np:int -> mem:int -> f:(np:int -> mem:int -> 'a Lwt.t) -> 'a Lwt.t
end =
struct

  type t = {
    np : int ;
    mem : int ;
    mutable current_np : int ;
    mutable current_mem : int ;
    mutable waiters : ((int * int) * unit Lwt.u) list ;
  }

  let create ~np ~mem = {
    np ; mem ;
    current_np = np ;
    current_mem = mem ;
    waiters = [] ;
  }

  let decr p ~np ~mem =
    p.current_np <- p.current_np - np ;
    p.current_mem <- p.current_mem - mem

  let incr p ~np ~mem =
    p.current_np <- p.current_np + np ;
    p.current_mem <- p.current_mem + mem

  let acquire p ~np ~mem =
    if np <= p.current_np && mem <= p.current_mem then (
      decr p ~np ~mem ;
      Lwt.return ()
    )
    else (
      let t, u = Lwt.wait () in
      p.waiters <- ((np,mem), u) :: p.waiters ;
      t
    )

  let release p ~np ~mem =
    let rec wake_guys_up p = function
      | [] -> []
      | (((np, mem), u) as h) :: t ->
        if np <= p.current_np && mem <= p.current_mem then (
          decr p ~np ~mem ;
          Lwt.wakeup u () ;
          t
        )
        else h :: (wake_guys_up p t)
    in
    incr p ~np ~mem ;
    p.waiters <- wake_guys_up p (List.sort (fun (x, _) (y,_) -> compare y x) p.waiters)

  let use p ~np ~mem ~f =
    if np > p.np then
      Lwt.fail (Invalid_argument "Bistro.Pool: asked more processors than there are in the pool")
    else if mem > p.mem then
      Lwt.fail (Invalid_argument "Bistro.Pool: asked more memory than there is in the pool")
    else (
      acquire p ~np ~mem >>= fun () ->
      Lwt.catch
        (fun () ->
           f ~np ~mem >>= fun r -> Lwt.return (`result r))
        (fun exn -> Lwt.return (`error exn))
      >>= fun r ->
      release p ~np ~mem ;
      match r with
      | `result r -> Lwt.return r
      | `error exn -> Lwt.fail exn
    )
end


type error = (Workflow.u * string) list
type 'a result = ('a, error) Result.t

type execution_report = {
  script : string ;
  exit_status : int ;
}

type backend =
  Db.t -> Workflow.step -> execution_report Lwt.t

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let interpreter_cmd path_to_script = function
  | `bash -> [ "bash" ; path_to_script ]
  | `ocaml -> [ "ocaml" ; path_to_script ]
  | `ocamlscript -> [ "ocamlscript" ; path_to_script ]
  | `python -> [ "python" ; path_to_script ]
  | `perl -> [ "perl" ; path_to_script ]
  | `R -> [ "Rscript" ; path_to_script ]
  | `sh -> [ "sh" ; path_to_script ]

let interpreter_cmd path_to_script interpreter =
  "", Array.of_list (interpreter_cmd path_to_script interpreter)

let extension_of_interpreter = function
  | `bash -> "sh"
  | `ocaml -> "ml"
  | `ocamlscript -> "ml"
  | `python -> "py"
  | `perl -> "pl"
  | `R -> "R"
  | `sh -> "sh"

let local_backend ~np ~mem : backend =
  let pool = Pool.create ~np ~mem in
  fun db ({ script ; } as step) ->
    Pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
        let interpreter = Script.interpreter script in
        match interpreter with
        | `sh | `bash | `R ->
          let stdout = Db.stdout_path db step in
          let stderr = Db.stderr_path db step in
          let dest = Db.build_path db step in
          let tmp = Db.tmp_path db step in
          let string_of_workflow = Db.workflow_path' db in
          let script_extension = extension_of_interpreter interpreter in
          let pkgvar _ var =
            let dir = Bistro.string_of_package_variable var in
            Filename.concat "/usr" dir
          in
          let script_file =
            Filename.temp_file "guizmin" ("." ^ script_extension) in
          let script_text =
            Script.to_string
              ~string_of_workflow
              ~np ~mem ~dest ~pkgvar ~tmp script in
          Lwt_io.(with_file
                    ~mode:output script_file
                    (fun oc -> write oc script_text)) >>= fun () ->
          redirection stdout >>= fun stdout ->
          redirection stderr >>= fun stderr ->
          let cmd = interpreter_cmd script_file interpreter in
          Lwt_process.exec ~stdout ~stderr cmd >>= fun status ->
          let exit_status = Caml.Unix.(match status with
              | WEXITED code
              | WSIGNALED code
              | WSTOPPED code -> code
            )
          in
          Lwt_unix.unlink script_file >>= fun () ->
          Lwt.return {
            script = script_text ;
            exit_status ;
          }
        | _ -> Lwt.fail (Failure ("Unsupported_interpreter: " ^ (extension_of_interpreter interpreter)))
      )


(* Currently Building Steps

     If two threads try to concurrently execute a step, we don't want
     the build procedure to be executed twice. So when the first
     thread tries to eval the workflow, we store the build thread in a
     hash table. When the second thread tries to eval, we give the
     build thread in the hash table, which prevents the workflow from
     being built twice concurrently.

*)
module CBST :
sig
  type t
  val create : unit -> t
  val find_or_add : t -> step -> (unit -> unit result Lwt.t) -> unit result Lwt.t
  val join : t -> unit Lwt.t
end
=
struct
  module S = struct
    type t = step
    let equal x y = x.id = y.id
    let hash x = String.hash x.id
  end

  module T = Caml.Hashtbl.Make(S)

  type contents =
    | Thread of unit result Lwt.t

  type t = contents T.t

  let create () = T.create 253


  let find_or_add table x f =
    let open Lwt in
    match T.find table x with
    | Thread t -> t
    | exception Not_found ->
      let waiter, u = Lwt.wait () in
      T.add table x (Thread waiter) ;
      Lwt.async (fun () ->
          f () >>= fun res ->
          T.remove table x ;
          Lwt.wakeup u res ;
          Lwt.return ()
        ) ;
      waiter

  let join table =
    let f _ (Thread t) accu = (Lwt.map ignore t) :: accu in
    T.fold f table []
    |> Lwt.join
end


type t = {
  db : Db.t ;
  backend : backend ;
  cbs : CBST.t ;
  mutable on : bool ;
}

let make backend db = {
  db ;
  backend = backend ;
  cbs = CBST.create () ;
  on = true ;
}

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-rf" ; fn |]) >>| ignore
  else
    Lwt.return ()

let join_results xs =
  let f accu x =
    x >>= function
    | Ok () -> Lwt.return accu
    | Error errors as e ->
      match accu with
      | Ok _ -> Lwt.return e
      | Error errors' -> Lwt.return (Error (errors @ errors'))
  in
  Lwt_list.fold_left_s f (Ok ()) xs


let rec build_workflow e = function
  | Input _ as i -> build_input e i
  | Select (_,dir,p) as x -> build_select e x dir p
  | Step step as u ->
    Db.requested e.db step ;
    let dest = Db.workflow_path' e.db u in
    if Sys.file_exists dest = `Yes then
      Lwt.return (Ok ())
    else
      CBST.find_or_add e.cbs step (fun () ->
          let dep_threads = List.map step.deps ~f:(build_workflow e) in
          build_step e step dep_threads
        )

and build_step
    e
    ({ np ; mem ; timeout ; script } as step)
    dep_threads =

  join_results dep_threads >>=? fun () ->
  (
    let stdout = Db.stdout_path e.db step in
    let stderr = Db.stderr_path e.db step in
    let dest = Db.build_path e.db step in
    let tmp = Db.tmp_path e.db step in
    remove_if_exists stdout >>= fun () ->
    remove_if_exists stderr >>= fun () ->
    remove_if_exists dest >>= fun () ->
    remove_if_exists tmp >>= fun () ->
    Lwt_unix.mkdir tmp 0o750 >>= fun () ->
    e.backend e.db step >>= fun { script ; exit_status } ->
    Db.Submitted_script_table.set e.db step script ;
    match exit_status, Sys.file_exists_exn dest with
    | 0, true ->
      remove_if_exists tmp >>= fun () ->
      Db.built e.db step ;
      Lwt_unix.rename dest (Db.cache_path e.db step) >>= fun () ->
      Lwt.return (Ok ())
    | 0, false ->
      let msg =
        "Workflow failed to produce its output at the prescribed location."
      in
      Lwt.return (Error [ Step step, msg ])
    | error_code, _ ->
      let msg = sprintf "Script exited with code %d" error_code in
      Lwt.return (Error [ Step step, msg ])
  )

and build_input e i =
  Lwt.wrap (fun () ->
      let p = Db.workflow_path' e.db i in
      if Sys.file_exists p <> `Yes then
        let msg =
          sprintf
            "File %s is declared as an input of a workflow but does not exist."
            p
        in
        Error [ i, msg ]
      else
        Ok ()
    )

and build_select e x dir p =
  let p = string_of_path p in
  let dir_path = Db.workflow_path' e.db dir in
  let check_in_dir () =
    if Sys.file_exists (Db.workflow_path' e.db x) <> `Yes
    then (
      let msg =
        sprintf "No file or directory named %s in directory workflow %s."
          p
          dir_path
      in
      Lwt.return (Error [ x, msg ])
    )
    else Lwt.return (Ok ())
  in
  if Sys.file_exists dir_path = `Yes then (
    check_in_dir () >>=? fun () ->
    let () = match dir with
      | Input _ -> ()
      | Select _ -> assert false
      | Step s -> Db.requested e.db s
    in
    Lwt.return (Ok ())
  )
  else (
    let dir_thread = build_workflow e dir in
    dir_thread >>=? check_in_dir
  )


let build' e u =
  (
    if e.on then
      build_workflow e u
    else
      Lwt.return (Error [u, "Engine_halted"])
  )
  >>= function
  | Ok () -> Lwt.return (Ok (Db.workflow_path' e.db u))
  | Error xs ->
    Lwt.return (Error xs)

let build_exn' e w =
  build' e w >>= function
  | Ok s -> Lwt.return s
  | Error xs ->
    let msgs = List.map ~f:(fun (w, msg) -> Workflow.id' w ^ "\t" ^ msg) xs in
    let msg = sprintf "Some build(s) failed:\n\t%s\n" (String.concat ~sep:"\n\t" msgs) in
    Lwt.fail (Failure msg)

let build e w = build' e (Workflow.u w)

let build_exn e w = build_exn' e (Workflow.u w)

let shutdown e =
  e.on <- false ;
  CBST.join e.cbs
