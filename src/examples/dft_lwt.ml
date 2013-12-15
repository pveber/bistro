open Core.Std
open Printf

let rec task i =
  let task_deps =
    List.init (i - 1) succ
    |> List.filter_map ~f:(
      fun j ->
	if j <> 1 && i mod j = 0 then Some (task j)
	else None
    )
  in
  Bistro_workflow.(
    make [
      L [ S"echo" ; I i ; S">" ; D ]
    ]
    |> fun init ->
      List.fold_right task_deps
	~init
	~f:(fun dep accu -> depends ~on:dep accu)
  )

let () =
  Lwt_unix.run (Bistro_concurrent.dryrun (Bistro_db.make "_bistro") (task 60))
