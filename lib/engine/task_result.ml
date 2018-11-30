open Core_kernel

type t =
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
  | Other of {
      id : string ;
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      summary : string ;
      msg : string option ;
    }

let id = function
  | Shell { id ; _ }
  | Other { id ; _ } -> id

let succeeded = function
  | Other { outcome ; _ }
  | Shell { outcome ; _ } -> (
      match outcome with
      | `Succeeded -> true
      | `Failed | `Missing_output -> false
    )

let error_short_descr = function
  | Shell x -> (
      match x.outcome with
      | `Missing_output -> "Missing output"
      | `Failed ->
        sprintf "Ended with exit code %d" x.exit_code
      | `Succeeded ->
        let msg = "Task_outcome.error_short_descr: not an error result" in
        raise (Invalid_argument msg)
    )
  | Other o -> o.summary

let error_long_descr x db buf id = match x with
  | Other o -> Option.iter o.msg ~f:(Buffer.add_string buf)
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
