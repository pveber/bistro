open Core.Std

let shell (log : _ Db.logger) s =
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


let exec db w =
  let foreach = Workflow.(function
    | Input p ->
      if Sys.file_exists p <> `Yes
      then failwithf "File %s is declared as an input of a workflow but does not exist." p ()

    | Select (_, p) as x ->
      if Sys.file_exists (Db.path db x) <> `Yes
      then failwithf "No file or directory named %s in directory workflow." p ()
    | Rule r as x ->
      Db.with_logger db x ~f:(fun log ->
	List.iter r.cmds (fun cmd ->
	  let tmp_path = Db.tmp_path db x in
	  let tokens = exec_cmd tmp_path (Db.path db) cmd in
	  let line = String.concat ~sep:" " tokens in
	  shell log line ;
	  Unix.rename ~src:tmp_path ~dst:(Db.cache_path db x)
	)
      )
  )
  in
  Db.setup db ;
  Workflow.depth_first_traversal
    ~init:()
    ~f:(fun w () -> foreach w)
    w
