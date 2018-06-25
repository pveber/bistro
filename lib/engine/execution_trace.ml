open Core_kernel

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Skipped of [ `Done_already
               | `Missing_dep
               | `Allocation_error of string ]

let is_errored = function
  | Run { outcome ; _ } -> not (Task_result.succeeded outcome)
  | Skipped (`Allocation_error _ | `Missing_dep) -> true
  | Skipped `Done_already -> false

let run ~ready ~start ~_end_ ~outcome = Run { ready ; start ; _end_ ; outcome }

let skipped x = Skipped x


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
  | Skipped (`Allocation_error err)->
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
  | Skipped (`Done_already | `Missing_dep) -> ()

let all_ok xs = not (List.exists ~f:is_errored xs)
