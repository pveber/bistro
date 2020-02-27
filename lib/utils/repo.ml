open Lwt.Infix
open Core
open Bistro
open Bistro_engine

module W = Bistro_internals.Workflow

type item =
  | Item  : string list * _ path workflow -> item
  | Item_list : {
      path : string list ;
      prefix : string ;
      ext : string option ;
      elts : _ path list workflow ;
    } -> item
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

let items path ~prefix ?ext elts = Item_list { path ; prefix ; ext ; elts }

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
  | Item_list l ->
    [%workflow
      let id = W.id (Private.reveal l.elts) in
      let elts = [%eval Workflow.spawn l.elts ~f:Workflow.path] in
      let n = List.length elts in
      let m = Float.(n |> of_int |> log10 |> to_int) in
      let ext = match l.ext with
        | None -> ""
        | Some s -> "." ^ s
      in
      let format =
        Scanf.format_from_string
          (sprintf {|%%s_%%0%dd%%s|} m)
          "%s%d%s" in
      let list_elt_path i =
        l.path @ [ sprintf format l.prefix i ext ]
      in
      List.mapi elts ~f:(fun i path_w ->
          normalized_repo_item ~repo_path:(list_elt_path i) ~id:(Misc.digest (id, i)) ~cache_path:path_w
        )]
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
      | Item _ | Item_list _ -> ()
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
      | Item_list l -> Item_list { l with path = prefix @ l.path}
      | Precious_item _ as i -> i
    )

let shift dir items = add_prefix [ dir ] items

let singleton dir w = [ [ dir ] %> w ]
