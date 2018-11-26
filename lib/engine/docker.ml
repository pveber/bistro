open Core
open Bistro_internals

let mount_options ~host_paths ~container_paths =
  List.map2_exn host_paths container_paths ~f:(fun h c ->
      sprintf "-v %s:%s" h c
    )
  |> String.concat ~sep:" "

let image_url image =
  sprintf "%s%s/%s%s"
    (Option.value_map ~default:"" ~f:(sprintf "%s/") image.Command.dck_registry)
    image.Command.dck_account
    image.Command.dck_name
    (Option.value_map ~default:"" ~f:(sprintf ":%s")  image.Command.dck_tag)

let chown_command ~path:dir ~uid =
  sprintf
    "docker run --log-driver=none --rm -v %s:/bistro -i busybox chown -R %d /bistro"
    dir uid
