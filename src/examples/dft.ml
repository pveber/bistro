open Core.Std
open Printf

let rec task i =
  let target_deps =
    List.init (i - 1) succ
    |> List.filter_map ~f:(
      fun j ->
	if i mod j = 0 then Some (task j)
	else None
    )
  in
  Bistro_workflow.(
    make Cmd.(script [
      cmd "echo" arg int i stdout_to dest
    ])
    |> fun init ->
      List.fold_left target_deps
	~init
	~f:(fun accu dep -> depends ~on:dep accu)
  )

let () = Bistro_export.to_script (Bistro_db.make "_bistro") (task 20) stdout
