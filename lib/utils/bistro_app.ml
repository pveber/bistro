open Core.Std
open Bistro.Std
open Bistro_engine

type 'a path = Path of string

let ( / ) (Path p) (Bistro.Selector q) =
  Path (p ^ (Bistro.string_of_path q))

type _ t =
  | Pure : 'a -> 'a t
  | PureW : 'a workflow * ('a path -> 'b) -> 'b t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | List : 'a t list -> 'a list t

let pure x = Pure x

let pureW w f = PureW (w, f)

let app f x = App (f, x)

let ( $ ) = app

let list xs = List xs

let rec to_workflow_list
  : type s. s t -> Bistro.any_workflow list
  = function
    | Pure _ -> []
    | PureW (w, _) -> [ Bistro.Workflow w ]
    | App (f, x) ->
      to_workflow_list f @ to_workflow_list x
    | List xs ->
      List.map xs ~f:to_workflow_list
      |> List.concat

let rec eval : type s. Db.t -> s t -> s
  = fun db app ->
    match app with
    | Pure x -> x
    | PureW (w, f) ->
      f (Path (Db.workflow_path db w))
    | App (f, x) ->
      (eval db f) (eval db x)
    | List xs ->
      List.map xs ~f:(eval db)


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


let has_error traces =
  String.Map.exists traces ~f:(function
      | Tdag_sig.Run { outcome = Error _ } -> true
      | _ -> false
    )

let run ?(use_docker = true) ?(np = 1) ?(mem = 1024) ?(verbose = false) app =
  let open Lwt in
  let main =
    let config = Task.config ~db_path:"_bistro" ~use_docker in
    let allocator = Allocator.create ~np ~mem in
    let workflows = to_workflow_list app in
    let log =
      if verbose then
        let logger = Bistro_console_logger.create () in
        Some (Bistro_console_logger.event logger)
      else
        None
    in
    Scheduler.run ?log config allocator workflows >>= fun traces ->
    if has_error traces then (
      error_report config.Task.db traces ;
      fail (Failure "Some workflow failed!")
    )
    else
      return (eval config.Task.db app)
  in
  Lwt_unix.run main

type repo_item =
  Repo_item : string list * _ workflow -> repo_item

let ( %> ) path w = Repo_item (path, w)

let make_absolute p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let link p p_u =
  let dst = Bistro.string_of_path p in
  let src = make_absolute p_u in
  Unix.mkdir_p (Filename.dirname dst) ;
  let cmd = sprintf "rm -rf %s && ln -s %s %s" dst src dst in
  ignore (Sys.command cmd)

let generate_page outdir (dest, Path cache_path) =
  link (outdir :: dest) cache_path

let foreach_target { Task.db } outdir traces (Repo_item (dest, w)) =
  let id = Bistro.Workflow.id w in
  match String.Map.find_exn traces id with
  | Tdag_sig.Run { outcome = Ok () }
  | Tdag_sig.Skipped `Done_already ->
    let cache_path = Db.cache db id in
    link (outdir :: dest) cache_path
  | Tdag_sig.Run { outcome = Error _ }
  | Tdag_sig.Skipped `Missing_dep -> ()

let of_repo ~outdir items =
  List.map items ~f:(function Repo_item (p, w) ->
      pure (generate_page outdir) $ pureW w (fun s -> p, s)
    )
  |> list
  |> app (pure ignore)
