open Core_kernel.Std

type path = string list

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let python_version fmt =
  let regexp = match fmt with
    | `M_m -> "[0-9]\\.[0-9]"
  in
  let ic = Unix.open_process_in (sprintf "python --version 2>&1 | grep -o '%s'" regexp) in
  let r = In_channel.input_line ic in
  In_channel.close ic ;
  r

module Infix = struct
  let ( % ) f g x = g (f x)
end
