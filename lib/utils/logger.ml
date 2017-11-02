let null = object
  method event _ _ _ = ()
  method stop = ()
  method wait4shutdown = Lwt.return ()
end

let tee l1 l2 = object
  method event x y z =
    l1#event x y z ;
    l2#event x y z

  method stop =
    l1#stop ; l2#stop

  method wait4shutdown =
    Lwt.join [ l1#wait4shutdown ; l2#wait4shutdown ]
end
