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
      L [ S"sleep" ; I 1 ] ;
      L [ S"echo" ; I i ; S">" ; D ] ;
      S"beep" ;
    ]
    |> fun init ->
      List.fold_right task_deps
	~init
	~f:(fun dep accu -> depends ~on:dep accu)
  )

let db = Bistro_db.make "_bistro"

let () = Bistro_db.setup db

let () =
  Lwt_unix.run (Bistro_concurrent.dryrun db (task 60))

let () =
  Lwt_unix.run (Bistro_concurrent.exec db (Bistro_concurrent.local_worker 1) (task 60))

