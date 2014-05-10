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
    make Shell_script.(
      begin_
	cmd "echo" arg int i stdout_to dest
      end_
    )
    |> fun init ->
      List.fold_left target_deps
	~init
	~f:(fun accu dep -> depends ~on:dep accu)
  )

let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

let _ = React.E.trace print_endline (Bistro_logger.to_strings logger)

let () = Bistro_sequential.exec db logger (task 20)

