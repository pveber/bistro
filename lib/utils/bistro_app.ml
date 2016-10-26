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

let error_short_descr =
  let open Task in
  function
  | Task.Input_doesn't_exist p ->
    sprintf "Input %s doesn't exist" p
  | Task.Invalid_select (dir, sel) ->
    sprintf "Path %s doesn't exist in %s" (Bistro.string_of_path sel) dir
  | Task.Step_failure { script ; exit_code ; dumps } ->
    sprintf "Ended with exit code %d" exit_code

let error_long_descr db tid =
  let open Task in
  function
  | Input_doesn't_exist _
  | Invalid_select _ -> ()
  | Step_failure { script ; exit_code ; dumps } ->
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| Submitted script                                                             |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" script ;
    List.iter dumps ~f:(fun (path, text) ->
        fprintf stderr "+------------------------------------------------------------------------------+\n" ;
        fprintf stderr "|> Dumped file: %s\n" path ;
        fprintf stderr "+------------------------------------------------------------------------------+\n" ;
        fprintf stderr "%s\n" text ;
      ) ;
    fprintf stderr "#\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| STDOUT                                                                       |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" (In_channel.read_all (Db.stdout db tid)) ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| STDERR                                                                       |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" (In_channel.read_all (Db.stderr db tid))

let error_report_aux db = function
  | tid, Scheduler.Run { outcome = Error e } ->
    let short_descr = error_short_descr e in
    fprintf stderr "################################################################################\n" ;
    fprintf stderr "#                                                                              #\n" ;
    fprintf stderr "#  Task %s failed\n" tid ;
    fprintf stderr "#                                                                               \n" ;
    fprintf stderr "#------------------------------------------------------------------------------#\n" ;
    fprintf stderr "#                                                                               \n" ;
    fprintf stderr "# %s\n" short_descr ;
    fprintf stderr "#                                                                              #\n" ;
    fprintf stderr "################################################################################\n" ;
    fprintf stderr "###\n" ;
    fprintf stderr "##\n" ;
    fprintf stderr "#\n" ;
    error_long_descr db tid e
  | _ -> ()

let error_report db traces =
  String.Map.to_alist traces
  |> List.iter ~f:(error_report_aux db)


let has_error traces =
  String.Map.exists traces ~f:Scheduler.(function
      | Run { outcome = Error _ }
      | Skipped (`Missing_dep | `Allocation_error _) -> true
      | Run { outcome = Ok _ }
      | Skipped `Done_already -> false
    )

let run
    ?(config = Task.config ~db_path:"_bistro" ~use_docker:true)
    ?(np = 1)
    ?(mem = 1024)
    ?log
    app =
  let open Lwt in
  let main =
    let allocator = Allocator.create ~np ~mem in
    let workflows = to_workflow_list app in
    let dag = Scheduler.compile workflows in
    Scheduler.(run ?log config allocator dag) >>= fun traces ->
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
  | Scheduler.Run { outcome = Ok () }
  | Scheduler.Skipped `Done_already ->
    let cache_path = Db.cache db id in
    link (outdir :: dest) cache_path
  | Scheduler.Run { outcome = Error _ }
  | Scheduler.Skipped (`Missing_dep | `Allocation_error _) -> ()

let of_repo ~outdir items =
  List.map items ~f:(function Repo_item (p, w) ->
      pure (generate_page outdir) $ pureW w (fun s -> p, s)
    )
  |> list
  |> app (pure ignore)
