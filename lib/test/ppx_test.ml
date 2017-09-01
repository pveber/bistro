open Core_kernel

let comment_filter bed = [%bistro_fun
  let open Core_kernel in
  In_channel.read_lines [%dep bed]
  |> List.filter ~f:(String.is_prefix ~prefix:"#")
  |> Out_channel.write_lines [%dest]
] (* ~descr:"comment_filter" ~np:1 () *)
