let null = object
  method event _ _ _ = ()
  method stop = ()
  method wait4shutdown = Lwt.return ()
end

let tee loggers = object
  method event x y z =
    List.iter (fun l -> l#event x y z) loggers

  method stop =
    List.iter (fun l -> l#stop) loggers

  method wait4shutdown =
    Lwt.join (List.map (fun l -> l#wait4shutdown) loggers)
end
