open Bistro_engine
open Core.Std

module T = struct
  include Task
  let sexp_of_t _ = assert false
  let t_of_sexp _ = assert false
  let hash t = String.hash (Task.id t)
end

module S = Hash_set.Make(T)

let color r g b a =
  let r = Int32.of_int_exn (r land 0xFF) in
  let g = g land 0xFF in
  let b = b land 0xFF in
  let a = a land 0xFF in
  Int32.bit_or
    (Int32.shift_left r 24)
    (Int32.of_int_exn (g lor b lor a))

let light_gray = 0xC0C0C0
let black = 0

let dot_output dag ~needed ~already_done fn =
  let vertex_attribute u =
    let open Task in
    let needed = Hash_set.mem needed u in
    let color = if needed then black else light_gray in
    match u with
    | Input (_, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Color color ; `Fontcolor color ; `Shape `Box ]
    | Select (_, _, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Fontcolor color ; `Color color ; `Shape `Box ]
    | Step { descr ; precious } as u ->
      let already_done  = Hash_set.mem already_done u in
      let label_suffix = if precious then "*" else "" in
      [ `Label (descr ^ label_suffix) ;
        `Shape `Box ;
        `Peripheries (if already_done then 2 else 1) ;
        `Color color ;
        `Fontcolor color ;
      ]
  in
  let edge_attribute (u, v) =
    let open Task in
    let style = match u, v with
      | Select _, Step _ -> [ `Style `Dotted ]
      | _ -> []
    in
    let color =
      if Hash_set.mem needed u
      && not (Hash_set.mem already_done u)
      then black else light_gray in
    style @ [ `Color color ]
  in
  Scheduler.DAG.dot_output dag vertex_attribute edge_attribute fn

class logger path : Scheduler.logger =
  object
    method event _ _ = function
      | Scheduler.Init { dag ; needed ; already_done } ->
        let needed = S.of_list needed in
        let already_done = S.of_list already_done in
        dot_output dag ~needed ~already_done path
      | _ -> ()

    method stop = ()

    method wait4shutdown = Lwt.return ()
  end

let create path = new logger path
