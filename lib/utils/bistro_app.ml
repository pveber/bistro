open Core.Std
open Bistro.Std
open Bistro_engine

type 'a path = Path of string

let ( / ) (Path p) (Bistro.Selector q) =
  Path (p ^ (Bistro.string_of_path q))

type _ t =
  | Pure : 'a -> 'a t
  | PureW : 'a workflow -> 'a path t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | List : 'a t list -> 'a list t

let pure x = Pure x

let pureW w = PureW w

let app f x = App (f, x)

let ( $ ) = app

let list xs = List xs

let assoc xs =
  let keys, terms = List.unzip xs in
  pure (fun values -> List.zip_exn keys values)
  $ list terms

let rec to_workflow_list
  : type s. s t -> Bistro.any_workflow list
  = function
    | Pure _ -> []
    | PureW w -> [ Bistro.Workflow w ]
    | App (f, x) ->
      to_workflow_list f @ to_workflow_list x
    | List xs ->
      List.map xs ~f:to_workflow_list
      |> List.concat

let rec eval : type s. Db.t -> s t -> s
  = fun db app ->
    match app with
    | Pure x -> x
    | PureW w ->
      Path (Db.workflow_path db w)
    | App (f, x) ->
      (eval db f) (eval db x)
    | List xs ->
      List.map xs ~f:(eval db)


module Syntax = struct
  module Let_syntax = struct
    type nonrec 'a t = 'a t
    let map x ~f = app (pure f) x
    let both x y =
      pure (fun x y -> x, y) $ x $ y
  end
end


let error_short_descr =
  let open Task in
  function
  | Task.Input_check { path } ->
    sprintf "Input %s doesn't exist" path
  | Select_check { dir_path ; sel } ->
    sprintf "Path %s doesn't exist in %s" (Bistro.string_of_path sel) dir_path
  | Step_result { exit_code } ->
    sprintf "Ended with exit code %d" exit_code

let error_long_descr db tid =
  let open Task in
  function
  | Input_check _
  | Select_check _ -> ()
  | Step_result { cmd ; exit_code ; dumps } ->
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "| Submitted script                                                             |\n" ;
    fprintf stderr "+------------------------------------------------------------------------------+\n" ;
    fprintf stderr "%s\n" cmd ;
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
  | tid, Scheduler.Run { outcome } when Task.failure outcome ->
    let short_descr = error_short_descr outcome in
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
    error_long_descr db tid outcome
  | _ -> ()

let error_report db traces =
  String.Map.to_alist traces
  |> List.iter ~f:(error_report_aux db)


let has_error traces =
  String.Map.exists traces ~f:Scheduler.(function
      | Run { outcome } -> Task.failure outcome
      | Skipped (`Missing_dep | `Allocation_error _) -> true
      | Skipped `Done_already -> false
    )

let run
    ?(np = 1)
    ?(mem = 1024)
    ?logger
    ?dag_dump
    app =
  let open Lwt in
  let main =
    let config = Task.config ~db_path:"_bistro" ~use_docker:true in
    let allocator = Allocator.create ~np ~mem in
    let workflows = to_workflow_list app in
    let dag = Scheduler.compile workflows in
    let () = match dag_dump with
      | None -> ()
      | Some fn ->
        Scheduler.DAG.dot_output dag fn
    in
    Scheduler.(run ?logger config allocator dag) >>= fun traces ->
    (
      match logger with
      | Some logger ->
        logger#stop ;
        logger#wait4shutdown
      | None -> Lwt.return ()
    ) >>= fun () ->
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

type normalized_repo_item = {
  repo_path  : string ;
  file_path  : string ;
  cache_path : string ;
}

type repo = normalized_repo_item list

let normalized_repo_item (Repo_item (repo_path, w)) (Path cache_path) =
  {
    repo_path = Bistro.string_of_path repo_path ;
    file_path = Filename.concat "_files" (Bistro.Workflow.id w) ;
    cache_path ;
  }

let ( %> ) path w = Repo_item (path, w)

let make_repo items =
  List.map items ~f:(fun (item, cache_path) ->
      normalized_repo_item item cache_path
    )

let is_strict_prefix ~prefix u =
  String.length prefix < String.length u
  && String.is_prefix ~prefix u

let find_bottom items item =
  let f min_item candidate =
    if is_strict_prefix ~prefix:candidate.cache_path min_item.cache_path
    then candidate
    else min_item
  in
  List.fold items ~init:item ~f

(* FIXME: quadratic complexity *)
let remove_redundancies repo =
  List.map repo ~f:(fun item ->
      let bottom = find_bottom repo item in
      if bottom = item then item
      else
        let cache_path =
          Filename.concat
            bottom.file_path
            (String.chop_prefix_exn ~prefix:bottom.cache_path item.cache_path)
        in
        { item with cache_path }
    )

let make_absolute p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let link dst p_u =
  let src = make_absolute p_u in
  Unix.mkdir_p (Filename.dirname dst) ;
  let cmd = sprintf "rm -rf %s && ln -r -s %s %s" dst src dst in
  ignore (Sys.command cmd)

let generate outdir items =
  List.iter items ~f:(fun item ->
      let repo_path = Filename.concat outdir item.repo_path in
      let file_path = Filename.concat outdir item.file_path in
      let cache_path =
        if Filename.is_absolute item.cache_path then item.cache_path
        else Filename.concat outdir item.cache_path in
      link repo_path file_path ;
      link file_path cache_path
    )

let of_repo ~outdir items =
  List.map items ~f:(function (Repo_item (p, w) as item) ->
      pure (normalized_repo_item item) $ (pureW w)
    )
  |> list
  |> app (pure remove_redundancies)
  |> app (pure (generate outdir))
