open Core.Std

let shell
    (log : _ Bistro_db.logger)
    ?(stdout = stdout)
    ?(stderr = stderr)
    s =
  log `debug "sh call:\n\n%s\n\n" s ;
  try
    Shell.call
      ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout))
      ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
      [ Shell.cmd "sh" [ "-c" ; s ] ]
  with Shell.Subprocess_error _ -> (
    log `error "sh call exited with non-zero code:\n\n%s\n\n" s ;
    Core.Std.failwithf "shell call failed:\n%s\n" s ()
  )

let remove_if_exists fn =
  if Sys.file_exists fn = `Yes
  then Sys.command_exn ("rm -r " ^ fn) |> ignore

let exec db w =
  let foreach = Bistro_workflow.(function
    | Input p ->
      if Sys.file_exists p <> `Yes
      then failwithf "File %s is declared as an input of a workflow but does not exist." p ()

    | Select (_, p) as x ->
      if Sys.file_exists (Bistro_db.path db x) <> `Yes
      then failwithf "No file or directory named %s in directory workflow." p ()
    | Rule r as x ->
      Bistro_db.with_logger db x ~f:(fun log ->
	Out_channel.with_file (Bistro_db.stdout_path db x) ~f:(fun stdout ->
	  Out_channel.with_file (Bistro_db.stderr_path db x) ~f:(fun stderr ->
	    List.iter r.cmds (fun cmd ->
	      let build_path = Bistro_db.build_path db x in
	      let tmp_path = Bistro_db.tmp_path db x in
	      let line = exec_cmd ~dest:build_path ~tmp:tmp_path (Bistro_db.path db) cmd in
	      remove_if_exists tmp_path ;
	      Sys.command_exn ("mkdir -p " ^ tmp_path) ;
	      shell ~stdout ~stderr log line ;
	      Unix.rename ~src:tmp_path ~dst:(Bistro_db.cache_path db x)
	    )
	  )
	)
      )
  )
  in
  Bistro_db.setup db ;
  Bistro_workflow.depth_first_traversal
    ~init:()
    ~f:(fun w () -> foreach w)
    w
