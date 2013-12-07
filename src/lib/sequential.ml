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
  let upath = Db.cache_path db in
  let foreach = Workflow.(function
    | Input p ->
      if Sys.file_exists p <> `Yes
      then failwithf "File %s is declared as an input of a workflow but does not exist." p ()

    | Select (dir, p) ->
      if Sys.file_exists (Filename.concat (upath dir) p) <> `Yes
      then failwithf "No file or directory named %s in directory workflow." p ()
    | Rule r as x ->
      Db.with_logger db x ~f:(fun log ->
	List.iter r.cmds (fun cmd ->
	  let tokens = exec_cmd (upath x) upath cmd in
	  let line = String.concat ~sep:" " tokens in
	  shell log line
	)
      )
  )
  in
  Db.setup db ;
  Workflow.depth_first_traversal
    ~init:()
    ~f:(fun w () -> foreach w)
    w
