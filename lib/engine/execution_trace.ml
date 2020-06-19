open Core_kernel

module Outcome = struct
  type t = [
    | `Succeeded
    | `Missing_output
    | `Error_exit_code of int
    | `Failure of string option
  ]

  let is_success = function
    | `Succeeded -> true
    | `Failure _ | `Missing_output | `Error_exit_code _ -> false
end

module Run_details = struct
  type t =
    | Input of { id : string ; path : string ; pass : bool }
    | Select of { id : string ; dir_path : string ; sel : string list ; pass : bool }
    | Shell of {
        id : string ;
        descr : string ;
        outcome : Outcome.t ;
        cmd : string ;
        file_dumps : Shell_command.file_dump list ;
        cache : string option ;
        stdout : string ;
        stderr : string ;
      }
    | Plugin of {
        id : string ;
        descr : string ;
        outcome : Outcome.t ;
      }
    | Container_image_fetch of {
        id : string ;
        outcome : (unit, [ `Singularity_failed_pull of int * string ]) result
      }

  let id = function
    | Input { id ;  _ }
    | Select { id ;  _}
    | Shell { id ; _ }
    | Plugin { id ; _ }
    | Container_image_fetch { id ; _ } -> id

  let name = function
    | Input { id ;  path ; _ } -> sprintf "input(%s, %s)" id path
    | Select { dir_path ; sel ; _ } ->
      sprintf "select(%s, %s)" dir_path (Path.to_string sel)
    | Shell { id ; descr ; _ } -> sprintf "shell(%s,%s)" descr id
    | Plugin { id ; descr ; _ } -> sprintf "plugin(%s,%s)" descr id
    | Container_image_fetch { id ; _ } -> sprintf "container_image_fetch(%s)" id

  let succeeded = function
    | Input { pass ; _ }
    | Select { pass ; _ } -> pass
    | Container_image_fetch { outcome = Ok (); _ } -> true
    | Container_image_fetch _ -> false
    | Plugin { outcome ; _ }
    | Shell { outcome ; _ } -> Outcome.is_success outcome

  let error_short_descr_of_outcome = function
    | `Missing_output -> "Missing output"
    | `Error_exit_code i ->
      sprintf "Ended with exit code %d" i
    | `Succeeded ->
      let msg = "Execution_trace.error_short_descr: not an error result" in
      raise (Invalid_argument msg)
    | `Failure (Some msg) -> sprintf "Failure: %s" msg
    | `Failure None -> "Failure"

  let error_short_descr = function
    | Input { path ; _ } -> sprintf "Input %s doesn't exist" path
    | Select { dir_path ; sel ; _ } ->
      sprintf "Path %s doesn't exist in %s" (Path.to_string sel) dir_path
    | Container_image_fetch _ -> sprintf "Container image could not be fetched"
    | Shell x -> error_short_descr_of_outcome x.outcome
    | Plugin o -> error_short_descr_of_outcome o.outcome

  let error_long_descr x db buf id = match x with
    | Input _ | Select _ -> ()
    | Plugin _ -> ()
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
    | Container_image_fetch x ->
      match x.outcome with
      | Ok () -> assert false
      | Error (`Singularity_failed_pull (_, url)) ->
        (
          bprintf buf "+------------------------------------------------------------------------------+\n" ;
          bprintf buf "| Image URL                                                                    |\n" ;
          bprintf buf "+------------------------------------------------------------------------------+\n" ;
          bprintf buf "%s\n" url
        )
end

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             details : Run_details.t }

  | Done_already of { id : string }
  | Canceled of {
      id : string ;
      missing_deps : t list ;
    }
  | Allocation_error of {
      id : string ;
      msg : string ;
    }

module S = struct
  module Elt = struct type nonrec t = t let compare = Poly.compare end
  include Caml.Set.Make(Elt)
end

let is_errored = function
  | Run { details ; _ } -> not (Run_details.succeeded details)
  | Allocation_error _
  | Canceled _ -> true
  | Done_already _ -> false

let gather_failures traces =
  List.fold traces ~init:S.empty ~f:(fun acc t ->
      match t with
      | Done_already _ -> acc
      | Run { details ; _ } ->
        if Run_details.succeeded details then
          acc
        else
          S.add t acc
      | Canceled { missing_deps ; _ } ->
        List.fold ~f:(Fn.flip S.add) ~init:acc missing_deps
      | Allocation_error _ -> S.add t acc
    )
  |> S.elements

let error_title buf title short_desc =
  bprintf buf "################################################################################\n" ;
  bprintf buf "#                                                                              #\n" ;
  bprintf buf "#  %s\n" title ;
  bprintf buf "#                                                                               \n" ;
  bprintf buf "#------------------------------------------------------------------------------#\n" ;
  bprintf buf "#                                                                               \n" ;
  bprintf buf "# %s\n" short_desc ;
  bprintf buf "#                                                                              #\n" ;
  bprintf buf "################################################################################\n" ;
  bprintf buf "###\n" ;
  bprintf buf "##\n" ;
  bprintf buf "#\n"

let error_report trace db buf =
  match trace with
  | Run { details ; _ } ->
    if not (Run_details.succeeded details) then
      let title = sprintf "Task %s failed\n" (Run_details.name details) in
      let short_descr = Run_details.error_short_descr details in
      error_title buf title short_descr ;
      Run_details.error_long_descr details db buf (Run_details.id details)
  | Allocation_error { id ; msg } ->
    let title = sprintf "Task %s failed\n" id in
    let short_descr = sprintf "Allocation error: %s\n" msg in
    error_title buf title short_descr
  | (Done_already _ | Canceled _) -> ()

let all_ok xs = not (List.exists ~f:is_errored xs)

module Set = S
