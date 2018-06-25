open Core_kernel

type t =
  | Input of { path : string ; pass : bool }
  | Select of { dir_path : string ; sel : string list ; pass : bool }
  | Shell of {
      id : string ;
      descr : string ;
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      exit_code : int ;
      cmd : string ;
      file_dumps : Shell_command.file_dump list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }
  | Closure of {
      id : string ;
      descr : string ;
      pass : bool
    }

let succeeded = function
  | Input { pass ; _ }
  | Select { pass ; _ }
  | Closure { pass ; _ } -> pass
  | Shell x -> (
      match x.outcome with
      | `Succeeded -> true
      | `Failed | `Missing_output -> false
    )

let error_short_descr = function
  | Input { path ; _ } -> sprintf "Input %s doesn't exist" path
  | Select { dir_path ; sel ; _ } ->
    sprintf "Path %s doesn't exist in %s" (Path.to_string sel) dir_path
  | Shell x -> (
      match x.outcome with
      | `Missing_output -> "Missing output"
      | `Failed ->
        sprintf "Ended with exit code %d" x.exit_code
      | `Succeeded ->
        let msg = "Task_outcome.error_short_descr: not an error result" in
        raise (Invalid_argument msg)
    )
  | Closure { descr ; _ } -> sprintf "Closure %s failed" descr

let error_long_descr x db buf id = match x with
  | Input _ | Select _ | Closure _ -> ()
  | Shell x ->
    (
      bprintf buf "+------------------------------------------------------------------------------+\n" ;
      bprintf buf "| Submitted script                                                             |\n" ;
      bprintf buf "+------------------------------------------------------------------------------+\n" ;
      bprintf buf "%s\n" x.cmd
    ) ;
    List.iter x.file_dumps ~f:(fun (Shell_command.File_dump { path ; text }) ->
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "|> Dumped file: %s\n" path ;
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "%s\n" text ;
      ) ;
    bprintf buf "#\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "| STDOUT                                                                       |\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "%s\n" (In_channel.read_all (Db.stdout db id)) ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "| STDERR                                                                       |\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "%s\n" (In_channel.read_all (Db.stderr db id))
