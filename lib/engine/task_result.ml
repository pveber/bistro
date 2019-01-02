open Core_kernel

type t =
  | Input of { id : string ; path : string ; pass : bool }
  | Select of { id : string ; dir_path : string ; sel : string list ; pass : bool }
  | Shell of {
      id : string ;
      descr : string ;
      outcome : [`Succeeded |
                 `Missing_output |
                 `Failed |
                 `Missing_container_image of string] ;
      exit_code : int ;
      cmd : string ;
      file_dumps : Shell_command.file_dump list ;
      cache : string option ;
      stdout : string ;
      stderr : string ;
    }
  | Plugin of {
      id : string ;
      descr : string ;
      outcome : [`Succeeded | `Missing_output | `Failed] ;
      msg : string option ;
    }

let id = function
  | Input { id ;  _ }
  | Select { id ;  _}
  | Shell { id ; _ }
  | Plugin { id ; _ } -> id

let succeeded_of_outcome = function
  | `Succeeded -> true
  | `Missing_container_image _
  | `Failed | `Missing_output -> false

let succeeded = function
  | Input { pass ; _ }
  | Select { pass ; _ } -> pass
  | Plugin { outcome ; _ } -> succeeded_of_outcome outcome
  | Shell { outcome ; _ } -> succeeded_of_outcome outcome

let error_short_descr = function
  | Input { path ; _ } -> sprintf "Input %s doesn't exist" path
  | Select { dir_path ; sel ; _ } ->
    sprintf "Path %s doesn't exist in %s" (Path.to_string sel) dir_path
  | Shell x -> (
      match x.outcome with
      | `Missing_output -> "Missing output"
      | `Missing_container_image _ -> sprintf "Container image could not be fetched"
      | `Failed ->
        sprintf "Ended with exit code %d" x.exit_code
      | `Succeeded ->
        let msg = "Task_outcome.error_short_descr: not an error result" in
        raise (Invalid_argument msg)
    )
  | Plugin o -> (
      match o.outcome with
      | `Missing_output -> "Missing output"
      | `Failed -> "Failed"
      | `Succeeded ->
        let msg = "Task_outcome.error_short_descr: not an error result" in
        raise (Invalid_argument msg)
    )

let error_long_descr x db buf id = match x with
  | Input _ | Select _ -> ()
  | Plugin o -> Option.iter o.msg ~f:(Buffer.add_string buf)
  | Shell x ->
    match x.outcome with
    | `Missing_container_image url ->
      (
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "| Image URL                                                                    |\n" ;
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "%s\n" url
      ) ;
    | `Missing_output | `Failed | `Succeeded ->
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
