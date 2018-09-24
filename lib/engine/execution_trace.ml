open Core_kernel
open Bistro_base

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Done_already of Task.t
  | Canceled of {
      task : Task.t ;
      missing_deps : t list ;
    }
  | Allocation_error of Task.t * string
  | Invalid_glob of {
      dir : Workflow.t ;
    }

module S = struct
  module Elt = struct type nonrec t = t let compare = compare end
  include Caml.Set.Make(Elt)
end

let is_errored = function
  | Run { outcome ; _ } -> not (Task_result.succeeded outcome)
  | Allocation_error _
  | Invalid_glob _
  | Canceled _ -> true
  | Done_already _ -> false

let gather_failures traces =
  List.fold traces ~init:S.empty ~f:(fun acc t ->
      match t with
      | Done_already _ -> acc
      | Run { outcome ; _ } ->
        if Task_result.succeeded outcome then
          acc
        else
          S.add t acc
      | Canceled { missing_deps ; _ } ->
        List.fold ~f:(Fn.flip S.add) ~init:acc missing_deps
      | Allocation_error _
      | Invalid_glob _ -> S.add t acc
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
  | Run { outcome ; _ } ->
    if not (Task_result.succeeded outcome) then
      let tid = Task_result.id outcome in
      let title = sprintf "Task %s failed\n" tid in
      let short_descr = Task_result.error_short_descr outcome in
      error_title buf title short_descr ;
      Task_result.error_long_descr outcome db buf tid
  | Allocation_error (w, err) ->
    let title = sprintf "Task %s failed\n" (Workflow.id w) in
    let short_descr = sprintf "Allocation error: %s\n" err in
    error_title buf title short_descr
  | Invalid_glob g ->
    let title = sprintf "Glob on dir %s failed\n" (Workflow.id g.dir) in
    let short_descr = "Glob failure\n" in
    error_title buf title short_descr
  | (Done_already _ | Canceled _) -> ()

let all_ok xs = not (List.exists ~f:is_errored xs)
