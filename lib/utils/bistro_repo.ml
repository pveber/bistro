open Core.Std
open Bistro.Std
open Bistro_app
open Bistro_engine

type item =
  Repo_item : string list * _ workflow -> item

type normalized_repo_item = {
  repo_path  : string ;
  file_path  : string ;
  cache_path : string ;
}

type repo = normalized_repo_item list

let normalized_repo_item (Repo_item (repo_path, w)) (Path cache_path) =
  {
    repo_path = Bistro.Path.to_string repo_path ;
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

let make_relative ~from p =
  let open Bistro.Path in
  make_relative ~from p
  |> to_string

let link dst p_u =
  let target = make_absolute p_u in
  let dst_dir = Filename.dirname dst in
  let target_from_dst_dir =
    make_relative ~from:(make_absolute dst_dir) (make_absolute target)
  in
  Unix.mkdir_p dst_dir ;
  let cmd = sprintf "rm -rf %s && ln -s %s %s" dst target_from_dst_dir dst in
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

let to_app ~outdir items =
  List.map items ~f:(function (Repo_item (p, w) as item) ->
      pure (normalized_repo_item item) $ (pureW w)
    )
  |> list
  |> app (pure remove_redundancies)
  |> app (pure (generate outdir))

let build ?np ?mem ?logger ?dag_dump ?keep_all ~outdir repo =
  Bistro_app.run ?np ?mem ?logger ?dag_dump ?keep_all (to_app ~outdir repo)
