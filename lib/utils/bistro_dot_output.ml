let dot_output dag fn =
  let vertex_attribute =
    let open Task in
    function
    | Input (_, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Color 0xFFFFFF ; `Shape `Box ]
    | Select (_, _, p) ->
      let label = Bistro.Path.to_string p in
      [ `Label label ; `Color 0xFFFFFF ; `Shape `Box ]
    | Step { descr } ->
      [ `Label descr ; `Shape `Box ]
  in
  let edge_attribute =
    let open Task in
    function
    | Select _, Step _ -> [ `Style `Dotted ]
    | _ -> []
  in
  Scheduler.DAG.dot_output dag vertex_attribute edge_attribute fn

class logger path : Scheduler.logger =
  object
    method event _ _ = function
      | Scheduler.Init { dag } ->
        dot_output dag path
      | _ -> ()

    method stop = ()

    method wait4shutdown = Lwt.return ()
  end

let create path = new logger path
