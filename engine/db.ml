open Core.Std
open Bistro
open Bistro.Workflow

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s

let ( >>=& ) x ~f =
  match x with
  | `Ok y -> `Ok y
  | `Error e -> `Error (f e)


(* this type should stay marshalable, because it is passed around in
   closures to workers. Not sure this can't be fixed, BTW*)
type t = string

let cache_dir db = Filename.concat db "cache"
let build_dir db = Filename.concat db "build"
let tmp_dir db = Filename.concat db "tmp"
let stderr_dir db = Filename.concat db "stderr"
let stdout_dir db = Filename.concat db "stdout"
let stats_path db = Filename.concat db "stats"

exception Corrupted_dbm

let with_dbm db f =
  match Dbm.(opendbm (stats_path db) [ Dbm_create ; Dbm_rdwr ] 0o700) with
  | dbh ->
    let r = match f dbh with
      | x -> `Ok x
      | exception exn -> `Error (`Exn exn)
    in
    Dbm.close dbh ;
    r
  | exception exn -> `Error `Corrupted_dbm

let with_dbm_exn db f =
  match with_dbm db f with
  | `Ok () -> ()
  | `Error `Corrupted_dbm ->
    failwithf "Corrupted db at %s (corrupted dbm records)" db ()
  | `Error (`Exn exn) -> raise exn

let assert_no_exn = function
  | `Exn _ -> assert false
  | `Malformed_db s -> `Malformed_db s
  | `Corrupted_dbm -> `Corrupted_dbm

let well_formed_db path =
  let open Pvem.Result in
  let check path =
    if Sys.file_exists_exn path then `Ok ()
    else `Error (`Malformed_db (path ^" doesn't exist"))
  in
  check path >>= fun () ->
  check (cache_dir path) >>= fun () ->
  check (build_dir path) >>= fun () ->
  check (tmp_dir path) >>= fun () ->
  check (stderr_dir path) >>= fun () ->
  check (stdout_dir path) >>= fun () ->
  check ((stats_path path) ^ ".pag") >>= fun () ->
  check ((stats_path path) ^ ".dir") >>= fun () ->
  with_dbm path (const ()) >>=&
  assert_no_exn

let init base =
  let open Pvem.Result in
  let base =
    if Filename.is_relative base then
      Filename.concat (Sys.getcwd ()) base
    else
      base
  in
  if Sys.file_exists_exn base
  then (
    well_formed_db base >>= fun () ->
    return base
  )
  else (
    Unix.mkdir_p (tmp_dir base) ;
    Unix.mkdir_p (build_dir base) ;
    Unix.mkdir_p (cache_dir base) ;
    Unix.mkdir_p (stderr_dir base) ;
    Unix.mkdir_p (stdout_dir base) ;
    with_dbm base ignore >>=&
    assert_no_exn >>= fun () ->
    return base
  )

let init_exn base =
  match init base with
  | `Ok db -> db
  | `Error e ->
    let explanation = match e with
      | `Corrupted_dbm -> "corrupted dbm"
      | `Malformed_db s -> s
    in
    failwithf
      "Bistro_db.init_exn: the path %s is not available for a bistro database or has been corrupted (%s)" base explanation ()

let aux_path f db step =
  Filename.concat (f db) step.id

let cache_path = aux_path cache_dir
let build_path = aux_path build_dir
let tmp_path = aux_path tmp_dir
let stdout_path = aux_path stdout_dir
let stderr_path = aux_path stderr_dir

module Stats = struct
  type t = {
    workflow : step ;
    history : (Time.t * event) list ;
    build_time : float option ;
  }
  and event = Built | Requested
  with sexp

  let make s = {
    workflow = s ;
    history = [] ;
    build_time = None ;
  }

  let load db step =
    match Dbm.find db step.id with
    | sexp -> Some (t_of_sexp (Sexp.of_string sexp))
    | exception Not_found -> None

  let save db step stats =
    Dbm.replace db step.id (Sexp.to_string (sexp_of_t stats))
end

let update_stats db step f =
  with_dbm_exn db (fun dbh ->
      let stat =
        match Stats.load dbh step with
        | Some s -> s
        | None -> Stats.make step
      in
      Stats.save dbh step (f stat)
    )
let append_history ~db u evt =
  update_stats db u (fun stat ->
      { stat with
        Stats.history = (Time.now (), evt) :: stat.Stats.history }
    )

let requested db step =
  append_history ~db step Stats.Requested

let built db step =
  append_history ~db step Stats.Built

let rec workflow_path' db = function
  | Extract (_, dir, p) ->
    Filename.concat (workflow_path' db dir) (string_of_path p)
  | Input (_, p) -> string_of_path p
  | Step step -> cache_path db step

let workflow_path db w = workflow_path' db (Workflow.u w)

let output_report_step db step oc =
  let dest = build_path db step in
  let tmp = tmp_path db step in
  let script = Script.to_string ~string_of_workflow:(workflow_path' db) ~dest ~tmp step.script in
  fprintf oc "################################################################################\n" ;
  fprintf oc "###\n" ;
  fprintf oc "##    Report on %s \n" step.id ;
  fprintf oc "#\n" ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "| Script                                                                       |\n" ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "%s" script ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "| STDOUT                                                                       |\n" ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "%s" (In_channel.read_all (stdout_path db step)) ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "| STDERR                                                                       |\n" ;
  fprintf oc "+------------------------------------------------------------------------------+\n" ;
  fprintf oc "%s" (In_channel.read_all (stderr_path db step))

let output_report db u oc = match u with
  | Input _ -> ()
  | Extract _ -> ()
  | Step step -> output_report_step db step oc

(* let log db fmt = *)
(*   let f msg = *)
(*     let path = *)
(*       Filename.concat *)
(*         (log_dir db) *)
(*         (Time.format (Time.now ()) "%Y-%m-%d.log") *)
(*     in *)
(*     echo ~path msg *)
(*   in *)
(*   Printf.ksprintf f fmt *)
