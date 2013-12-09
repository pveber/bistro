open Core.Std
open Printf

let rec task i =
  let deps =
    List.init (i - 1) succ
    |> List.filter_map ~f:(
      fun j ->
	if i mod j = 0 then Some (task j)
	else None
    )
  in
  Bistro_workflow.(
    make [
      L [ S"echo" ; I i ; S">" ; D ]
    ]
    |> fun init ->
      List.fold_left deps
	~init
	~f:(fun accu dep -> depends ~on:dep accu)
  )

let () = Bistro_export.to_script (Bistro_db.make "_bistro") (task 20) stdout
