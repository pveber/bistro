open Core.Std
open Bistro.Std
open Bistro_engine2
open Lwt

type target = Target : string list * _ workflow -> target

type plan = target list

let ( %> ) path w = Target (path, w)

let rec string_of_path = function
  | [] -> "."
  | "" :: t -> Filename.concat "." (string_of_path t)
  | p -> List.reduce_exn p ~f:Filename.concat

let make_absolute p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let link p p_u =
  let dst = string_of_path p in
  let src = make_absolute p_u in
  Unix.mkdir_p (Filename.dirname dst) ;
  let cmd = sprintf "rm -rf %s && ln -s %s %s" dst src dst in
  ignore (Sys.command cmd)

let foreach_target { Task.db } outdir traces (Target (dest, w)) =
  let id = Bistro.Workflow.id w in
  match String.Map.find_exn traces id with
  | Tdag_sig.Run { outcome = Ok () }
  | Tdag_sig.Skipped `Done_already ->
    let cache_path = Db.cache db id in
    link (outdir :: dest) cache_path
  | Tdag_sig.Run { outcome = Error _ }
  | Tdag_sig.Skipped `Missing_dep -> ()


let error_report_aux db = function
  | tid, Tdag_sig.Run { outcome = Error (`Msg msg) } ->
    fprintf stderr "################################################################################\n" ;
    fprintf stderr "#                                                                              #\n" ;
    fprintf stderr "#  Task %s failed\n" tid ;
    fprintf stderr "#                                                                               \n" ;
    fprintf stderr "#------------------------------------------------------------------------------#\n" ;
    fprintf stderr "#                                                                               \n" ;
    fprintf stderr "# %s\n" msg ;
    fprintf stderr "#                                                                              #\n" ;
    fprintf stderr "################################################################################\n" ;
    fprintf stderr "###\n" ;
    fprintf stderr "##    Report on %s \n" tid ;
    fprintf stderr "#\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| STDOUT                                                                       |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" (In_channel.read_all (Db.stdout db tid)) ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| STDERR                                                                       |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" (In_channel.read_all (Db.stderr db tid)) ;
  | _ -> ()

let error_report db traces =
  String.Map.to_alist traces
  |> List.iter ~f:(error_report_aux db)



let local ?(use_docker = true) ?(np = 1) ?(mem = 1024) ~outdir targets =
  let main =
    let config = Task.config ~db_path:"_bistro" ~use_docker in
    let allocator = Allocator.create ~np ~mem in
    let workflows = List.map targets ~f:(fun (Target (_, w)) -> Bistro.Workflow w) in
    Scheduler.run config allocator workflows >>= fun traces ->
    List.iter targets ~f:(foreach_target config outdir traces) ;
    error_report config.Task.db traces ;
    return ()
  in
  Lwt_unix.run main
