open Core
open Bistro.Std

type item =
  Repo_item : string list * _ workflow -> item

type t = item list

type normalized_repo_item = {
  repo_path  : string ;
  file_path  : string ;
  target_path : string ;
  target_in_cache : bool ;
}

let normalized_repo_item (Repo_item (repo_path, w)) (Term.Path target_path) =
  {
    repo_path = Bistro.Path.to_string repo_path ;
    file_path = Filename.concat "_files" (Bistro.Workflow.id w) ;
    target_path ;
    target_in_cache = true ;
  }

let item path w = Repo_item (path, w)

let ( %> ) path w = item path w

let find_bottom items item =
  let module P = Bistro.Path in
  let f min_item candidate =
    if
      P.is_strict_prefix
        ~prefix:(P.of_string candidate.target_path)
        (P.of_string min_item.target_path)
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
        let target_path =
          Filename.concat
            bottom.file_path
            (String.chop_prefix_exn ~prefix:bottom.target_path item.target_path)
        in
        { item with target_path ; target_in_cache = false }
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
  let outdir = if Filename.is_absolute outdir then outdir else make_absolute outdir in
  List.iter items ~f:(fun item ->
      let repo_path = Filename.concat outdir item.repo_path in
      let file_path = Filename.concat outdir item.file_path in
      let target_path =
        if item.target_in_cache then item.target_path
        else Filename.concat outdir item.target_path in
      link repo_path file_path ;
      link file_path target_path
    )

let use t (Bistro.Any_workflow w) =
  Term.(pure (fun x _ -> x) $ t $ pureW w)

let to_term ?(precious = []) ~outdir items =
  let open Term in
  List.map items ~f:(function (Repo_item (_, w) as item) ->
      pure (normalized_repo_item item) $ (pureW w)
    )
  |> list
  |> app (pure remove_redundancies)
  |> app (pure (generate outdir))
  |> fun init -> List.fold precious ~init ~f:use

let build ?np ?mem ?logger ?keep_all ?use_docker ?precious ?bistro_dir ~outdir repo =
  Term.run ?np ?mem ?logger ?keep_all ?use_docker ?bistro_dir (to_term ~outdir ?precious repo)

let dry_run ?precious repo =
  Term.dry_run (to_term ~outdir:"res" ?precious repo)

let add_prefix prefix items =
  List.map items ~f:(function
      | Repo_item (p, w) -> Repo_item (prefix @ p, w)
    )

let shift dir items = add_prefix [ dir ] items

let singleton dir w = [ [ dir ] %> w ]
