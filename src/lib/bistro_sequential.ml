open Core.Std

let shell
    (logger : Bistro_logger.t)
    ?(stdout = stdout)
    ?(stderr = stderr)
    script =
  let script_file = Filename.temp_file "bistro" ".sh" in
  Bistro_logger.debug logger "sh call:\n\n%s\n\n" script ;
  Out_channel.write_all script_file ~data:script ;
  try
    Shell.call
      ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout))
      ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
      [ Shell.cmd "sh" [ script_file ] ] ;
    Unix.unlink script_file ;
  with Shell.Subprocess_error _ -> (
    Bistro_logger.error logger "shell script %s failed\n" script_file ;
    Core.Std.failwithf "shell script %s failed" script_file ()
  )

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes
  then Sys.command_exn ("rm -r " ^ fn) |> ignore

let exec db logger w =
  let foreach = Bistro_workflow.(function
    | Input p ->
      if Sys.file_exists p <> `Yes
      then failwithf "File %s is declared as an input of a workflow but does not exist." p ()

    | Select (_, p) as x ->
      if Sys.file_exists (Bistro_db.path db x) <> `Yes
      then failwithf "No file or directory named %s in directory workflow." p ()
    | Rule r as x ->
      Out_channel.with_file (Bistro_db.stdout_path db x) ~f:(fun stdout ->
	Out_channel.with_file (Bistro_db.stderr_path db x) ~f:(fun stderr ->
	  let build_path = Bistro_db.build_path db x in
	  let tmp_path = Bistro_db.tmp_path db x in
	  let script = script_to_string ~dest:build_path ~tmp:tmp_path (Bistro_db.path db) r.script in
	  remove_if_exists tmp_path ;
	  Sys.command_exn ("mkdir -p " ^ tmp_path) ;
	  Bistro_logger.started logger x ;
	  shell ~stdout ~stderr logger script ;
	  Bistro_logger.finished logger x ;
	  Unix.rename ~src:build_path ~dst:(Bistro_db.path db x)
	)
      )
  )
  in
  Bistro_workflow.depth_first_traversal
    ~init:()
    ~f:(fun w () -> foreach w)
    w
