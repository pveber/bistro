open Lwt.Infix
open Core
open Bistro
open Bistro_engine

module W = Bistro_internals.Workflow

type item =
  | Item  : string list * _ path workflow -> item
  | Precious_item : _ path workflow -> item

type t = item list

type normalized_repo_item = {
  repo_path  : string ;
  file_path  : string ;
  cache_path : string ;
}

let normalized_repo_item ~repo_path ~id ~cache_path = {
    repo_path = Path.to_string repo_path ;
    file_path = Filename.concat "_files" id ;
    cache_path ;
  }

let item path w = Item (path, w)

let precious_item w = Precious_item w

let ( %> ) path w = item path w

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
      if Poly.(bottom = item) then item
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

let make_relative ~from p =
  let open Path in
  make_relative ~from p
  |> to_string

let link dst p_u =
  let target = make_absolute p_u in
  let dst_dir = Filename.dirname dst in
  let target_from_dst_dir =
    make_relative ~from:(make_absolute dst_dir) (make_absolute target)
  in
  Unix.mkdir_p dst_dir ;
  let cmd = sprintf "rm -rf '%s' && ln -s '%s' '%s'" dst target_from_dst_dir dst in
  ignore (Sys.command cmd)

let generate outdir items =
  let items = remove_redundancies items in
  List.iter items ~f:(fun item ->
      let repo_path = Filename.concat outdir item.repo_path in
      let file_path = Filename.concat outdir item.file_path in
      let cache_path =
        if Filename.is_absolute item.cache_path then item.cache_path
        else Filename.concat outdir item.cache_path in
      link repo_path file_path ;
      link file_path cache_path
    )

let item_to_workflow = function
  | Item (path, w) ->
    [%workflow [ normalized_repo_item ~repo_path:path ~id:(W.id (Private.reveal w)) ~cache_path:[%path w] ]]
  | Precious_item _ -> Workflow.data []

let to_workflow ~outdir items =
  let normalized_items =
    List.map items ~f:item_to_workflow
    |> Workflow.list
  in
  [%workflow
    [%eval normalized_items]
    |> List.concat
    |> remove_redundancies
    |> generate outdir]

let partition_results xs =
  let rec inner ok err = function
    | [] -> ok, err
    | Ok x :: t -> inner (x :: ok) err t
    | Error e :: t -> inner ok (e :: err) t
  in
  inner [] [] xs

let protect sched items =
  List.iter items ~f:(function
      | Precious_item w -> Scheduler.protect sched w
      | Item _ -> ()
    )

let build ?np ?mem ?loggers ?allowed_containers ?(bistro_dir = "_bistro") ?collect ~outdir repo =
  let db = Db.init_exn bistro_dir in
  let expressions = List.map repo ~f:(item_to_workflow) in
  let sched = Scheduler.create ?np ?mem ?loggers ?allowed_containers ?collect db in
  protect sched repo ;
  let results = Lwt_list.map_p (Scheduler.eval sched) expressions in
  Scheduler.start sched ;
  Lwt.map partition_results results >>= fun (res, errors) ->
  Scheduler.stop sched >|= fun () ->
  generate outdir (List.concat res) ;
  match errors with
  | [] -> ()
  | _ :: _ -> (
    let errors =
      List.concat errors
      |> Execution_trace.gather_failures
    in
    prerr_endline (Scheduler.error_report sched errors) ;
    failwith "Some workflow failed!"
  )

let build_main ?np ?mem ?loggers ?allowed_containers ?bistro_dir ?collect ~outdir repo =
  build ?np ?mem ?loggers ?allowed_containers ?bistro_dir ?collect ~outdir repo
  |> Lwt_main.run

let add_prefix prefix items =
  List.map items ~f:(function
      | Item  (p, w) -> Item  (prefix @ p, w)
      | Precious_item _ as i -> i
    )

let shift dir items = add_prefix [ dir ] items

let singleton dir w = [ [ dir ] %> w ]

let protected_set repo =
  let rec fold_path_workflow acc (W.Any w) =
    match w with
    | Select s -> fold_path_workflow acc (W.Any s.dir)
    | Input _ -> acc
    | Shell _
    | Plugin _ -> String.Set.add acc (W.id w)
    | App _
    | Both _
    | Eval_path _
    | Glob _
    | List _
    | List_nth _
    | Pure _
    | Spawn _ -> assert false
  in
  let k acc w = fold_path_workflow acc (W.Any (Bistro.Private.reveal w)) in
  List.fold repo ~init:String.Set.empty ~f:(fun acc it ->
      match it with
      | Item (_, w) -> k acc w
      | Precious_item w -> k acc w
    )

let cache_clip_fold ~bistro_dir repo ~f ~init =
  let protected = protected_set repo in
  let db = Db.init_exn bistro_dir in
  Db.fold_cache db ~init ~f:(fun acc fn ->
      let id = Filename.basename fn in
      f acc (if String.Set.mem protected id then `Protected fn else `Unprotected fn)
    )

let cache_clip_dry_run ~bistro_dir repo =
  cache_clip_fold ~bistro_dir repo ~init:(0,0,0,0) ~f:(fun (total_files, total_size, deleted_files, deleted_size) file ->
      let fn, protected = match file with
        | `Protected fn -> fn, true
        | `Unprotected fn -> fn, false
      in
      match Misc.du fn with
      | Ok size ->
        let total_files = total_files + 1 in
        let total_size = total_size + size in
        if protected then (total_files, total_size, deleted_files, deleted_size)
        else (total_files, total_size, deleted_files + 1, deleted_size + size)
      | Error (`Msg msg) ->
        failwithf "du: %s" msg ()
    )

let cache_clip ~bistro_dir repo =
  cache_clip_fold ~bistro_dir repo ~init:() ~f:(fun () file ->
      match file with
      | `Protected _ -> ()
      | `Unprotected fn ->
        match Misc.rm_rf fn with
        | Ok () -> ()
        | Error (`Msg msg) -> failwithf "rm: %s" msg ()
    )
