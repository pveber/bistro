open Core.Std
open Lwt
open Bistro
open Bistro.Workflow

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s


type t = {
  path : string ;
  dbh : Dbm.t ; (* DBM handler *)
}

let cache_dir base = Filename.concat base "cache"
let build_dir base = Filename.concat base "build"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"
let stats_prefix base = Filename.concat base "stats"

let open_dbm ?(mode = Dbm.[ Dbm_create ; Dbm_rdwr ]) path =
  match Dbm.opendbm path mode 0o700 with
  | dbh -> `Ok dbh
  | exception exn -> `Error `Corrupted_dbm

let with_dbm ?mode path f =
  match open_dbm ?mode path with
  | `Ok dbh -> (
    let r = match f dbh with
      | x -> `Ok x
      | exception exn -> `Error exn
    in
    Dbm.close dbh ;
    match r with
    | `Ok x -> `Ok x
    | `Error exn -> raise exn
    )
  | `Error _ as e -> e

let with_dbm_exn ?mode db f =
  match with_dbm ?mode db f with
  | `Ok () -> ()
  | `Error `Corrupted_dbm ->
    failwithf "Corrupted db at %s (corrupted dbm records)" db ()

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
  check ((stats_prefix path) ^ ".pag") >>= fun () ->
  check ((stats_prefix path) ^ ".dir") >>= fun () ->
  with_dbm path ignore

let check_path path =
  match Sys.file_exists path with
  | `Yes ->
    well_formed_db path
  | `No ->
    Unix.mkdir_p (tmp_dir path) ;
    Unix.mkdir_p (build_dir path) ;
    Unix.mkdir_p (cache_dir path) ;
    Unix.mkdir_p (stderr_dir path) ;
    Unix.mkdir_p (stdout_dir path) ;
    with_dbm path ignore
  | `Unknown -> `Error `Path_is_not_reachable

let open_ path =
  let open Pvem.Result in
  let path =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  check_path path
  >>= fun () -> open_dbm path
  >>| fun dbh -> { path ; dbh }

let raise_error path e =
  let explanation = match e with
    | `Path_is_not_reachable -> "unreadable path"
    | `Corrupted_dbm -> "corrupted dbm"
    | `Malformed_db s -> s
  in
  failwithf
    "Path %s is not available for a bistro database or has been corrupted (%s)"
    path explanation ()

let to_exn f x =
  match f x with
  | `Ok y -> y
  | `Error e -> raise_error x e

let open_exn path = to_exn open_ path

let close db = Dbm.close db.dbh

let with_open path f =
  let open Lwt in
  match open_ path with
  | `Ok db ->
    finalize
      (fun () -> f db >|= fun x -> `Ok x)
      (fun () -> close db ; return ())
  | `Error e -> return (`Error e)

let with_open_exn path f =
  let open Lwt in
  with_open path f >|= function
  | `Ok x -> x
  | `Error e -> raise_error path e

let aux_path f db step =
  Filename.concat (f db.path) step.id

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

let fold db ~init ~f =
  let res = ref init in
  let f key data =
    res := f !res (Stats.t_of_sexp (Sexp.of_string data))
  in
  Dbm.iter f db.dbh ;
  !res


let update_stats db step f =
  let stat =
    match Stats.load db.dbh step with
    | Some s -> s
    | None -> Stats.make step
  in
  Stats.save db.dbh step (f stat)


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
  | Select (_, dir, p) ->
    Filename.concat (workflow_path' db dir) (string_of_path p)
  | Input (_, p) -> string_of_path p
  | Step step -> cache_path db step

let workflow_path db w = workflow_path' db (Workflow.u w)

let report_step db step =
  let buf = Buffer.create 251 in
  let dest = build_path db step in
  let tmp = tmp_path db step in
  let script = Script.to_string ~string_of_workflow:(workflow_path' db) ~dest ~tmp step.script in
  bprintf buf "################################################################################\n" ;
  bprintf buf "###\n" ;
  bprintf buf "##    Report on %s \n" step.id ;
  bprintf buf "#\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| Script                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" script ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDOUT                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" (In_channel.read_all (stdout_path db step)) ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "| STDERR                                                                       |\n" ;
  bprintf buf "+------------------------------------------------------------------------------+\n" ;
  bprintf buf "%s" (In_channel.read_all (stderr_path db step)) ;
  Buffer.contents buf

let report db = function
  | Input _ -> ""
  | Select _ -> ""
  | Step step -> report_step db step

let output_report db u oc = match u with
  | Input _ -> ()
  | Select _ -> ()
  | Step step -> output_string oc (report_step db step)

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
