open Core_kernel

include File_formats

open Bistro
open Shell_dsl

module Cmd = struct
  let wget ?no_check_certificate ?user ?password ?dest url =
    cmd "wget" [
      option (flag string "--no-check-certificate") no_check_certificate ;
      option (opt "--user" string) user ;
      option (opt "--password" string) password ;
      option (opt "-O" ident) dest ;
      string url ;
    ]
end

let wget ?descr_url ?no_check_certificate ?user ?password url =
  let info = match descr_url with None -> "" | Some i -> sprintf "(%s)" i in
  shell ~descr:("utils.wget" ^ info) [
    Cmd.wget ?no_check_certificate ?user ?password ~dest url
  ]

let unzip zip =
  shell ~descr:"utils.unzip" [
    cmd "unzip" [ opt "-d" ident dest ; dep zip ]
  ]

let gunzip gz =
  shell ~descr:"utils.gunzip" [
    cmd "gunzip" [ opt "-c" dep gz ] ~stdout:dest
  ]

let bunzip2 bz2 =
  shell ~descr:"utils.bunzip2" [
    cmd "bunzip2" [ opt "-c" dep bz2 ] ~stdout:dest
  ]

let tar_xfz ?strip_components tgz =
  shell ~descr:"utils.tar_xfz" [
    mkdir_p dest ;
    cmd "tar" [
      string "xfz" ;
      dep tgz ;
      opt "-C" ident dest ;
      option (opt "--strip-components" int) strip_components ;
    ] ;
  ]

let crlf2lf f =
  shell ~descr:"utils.crlf2lf" [
    cmd "tr" [ opt "-d" string "'\r'"] ~stdin:(dep f) ~stdout:dest
  ]
