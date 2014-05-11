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

let db = Bistro_db.init "_bistro"
let blog = Bistro_log.make ~db ~hook:(fun x -> print_endline (Bistro_log.Entry.to_string x)) ()
let backend = Bistro_engine.local_worker blog

let () = Bistro_engine.run db blog backend (task 20)

