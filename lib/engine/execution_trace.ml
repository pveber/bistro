open Core_kernel
open Bistro_base

module S = String.Set

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Done_already
  | Canceled of { missing_deps : String.Set.t }
  | Allocation_error of string

let is_errored = function
  | Run { outcome ; _ } -> not (Task_result.succeeded outcome)
  | Allocation_error _
  | Canceled _ -> true
  | Done_already -> false

let all_ok xs = not (List.exists ~f:is_errored xs)

  let gather_failures workflows traces =
    List.fold2_exn workflows traces ~init:S.empty ~f:(fun acc w t ->
        match t with
        | Done_already -> acc
        | Run { outcome ; _ } ->
          if Task_result.succeeded outcome then
            acc
          else
            S.add acc (Workflow.id w)
        | Canceled { missing_deps } -> S.union acc missing_deps
        | Allocation_error _ -> S.add acc (Workflow.id w)
      )

let error_report trace db buf tid =
  match trace with
  | Run { outcome ; _ } ->
    if not (Task_result.succeeded outcome) then
      let short_descr = Task_result.error_short_descr outcome in
      bprintf buf "################################################################################\n" ;
      bprintf buf "#                                                                              #\n" ;
      bprintf buf "#  Task %s failed\n" tid ;
      bprintf buf "#                                                                               \n" ;
      bprintf buf "#------------------------------------------------------------------------------#\n" ;
      bprintf buf "#                                                                               \n" ;
      bprintf buf "# %s\n" short_descr ;
      bprintf buf "#                                                                              #\n" ;
      bprintf buf "################################################################################\n" ;
      bprintf buf "###\n" ;
      bprintf buf "##\n" ;
      bprintf buf "#\n" ;
      Task_result.error_long_descr outcome db buf tid
  | Allocation_error err ->
    bprintf buf "################################################################################\n" ;
    bprintf buf "#                                                                              #\n" ;
    bprintf buf "#  Task %s failed\n" tid ;
    bprintf buf "#                                                                               \n" ;
    bprintf buf "#------------------------------------------------------------------------------#\n" ;
    bprintf buf "#                                                                               \n" ;
    bprintf buf "# Allocation error: %s\n" err ;
    bprintf buf "#                                                                              #\n" ;
    bprintf buf "################################################################################\n" ;
    bprintf buf "###\n" ;
    bprintf buf "##\n" ;
    bprintf buf "#\n"
  | (Done_already | Canceled _) -> ()

let all_ok xs = not (List.exists ~f:is_errored xs)
