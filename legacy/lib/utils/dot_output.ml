open Bistro_engine
open Core

module T = struct
  include Task
  let sexp_of_t _ = assert false
  let t_of_sexp _ = assert false
  let hash t = String.hash (Task.id t)
end

module S = Hash_set.Make(T)

let light_gray = 0xC0C0C0
let black = 0

let shape = function
  | Bistro.Input _
  | Select _
  | Step _ -> `Box

let dot_output dag ~needed ~already_done ~precious fn =
  let vertex_attribute u =
    let open Bistro in
    let needed = Hash_set.mem needed u in
    let color = if needed then black else light_gray in
    let shape = `Shape (shape u) in
    match u with
    | Input (_, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Color color ; `Fontcolor color ; shape ]
    | Select (_, _, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Fontcolor color ; `Color color ; shape ]
    | Step { descr ; id ; _ } as u ->
      let already_done  = Hash_set.mem already_done u in
      let precious = String.Set.mem precious id in
      let label_suffix = if precious then "*" else "" in
      [ `Label (descr ^ label_suffix) ;
        shape ;
        `Peripheries (if already_done then 2 else 1) ;
        `Color color ;
        `Fontcolor color ;
      ]
  in
  let edge_attribute (u, v) =
    let open Bistro in
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
    method event config _ = function
      | Scheduler.Init { dag ; needed ; already_done } ->
        let needed = S.of_list needed in
        let already_done = S.of_list already_done in
        dot_output dag ~needed ~already_done path ~precious:config.Task.precious
      | _ -> ()

    method stop = ()

    method wait4shutdown = Lwt.return ()
  end

let create path = new logger path
