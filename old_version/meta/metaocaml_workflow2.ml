#use "topfind"
#require "core_kernel"

open Core_kernel.Std

let digest x =
  let buf = Buffer.create 253 in
  let format = Format.formatter_of_buffer buf in
  Print_code.print_code format x ;
  Digest.(to_hex (string (Buffer.contents buf)))


let cache_code x =
  let id = digest x in
  .<
    if Sys.file_exists id then
      In_channel.with_file id ~f:(Marshal.from_channel)
    else
      let y = .~x in
      Out_channel.with_file id ~f:(fun oc ->
	Marshal.to_channel oc []
      ) ;
      y
  >.

let s_addition = .< fun x y -> x + y >.

let () =
  Print_code.print_code
    Format.std_formatter
    (cache_code .< .~s_addition 2 .~(.<2>.) >.)



type 'a workflow = < build : (unit -> unit) code ; get : unit -> 'a >

