let null = object
  method event _ _ = ()
  method stop = ()
  method wait4shutdown = Lwt.return ()
end

let tee l1 l2 = object
  method event x y =
    l1#event x y ;
    l2#event x y

  method stop =
    l1#stop ; l2#stop

  method wait4shutdown =
    Lwt.join [ l1#wait4shutdown ; l2#wait4shutdown ]
end
