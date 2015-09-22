open Core_kernel.Std
open Types
open Bistro.EDSL_sh

let wget ?descr_url ?no_check_certificate url =
  let info = match descr_url with None -> "" | Some i -> sprintf "(%s)" i in
  workflow ~descr:("utils.wget" ^ info) [
    cmd "wget" [
      option (flag string "--no-check-certificate") no_check_certificate ;
      opt "-O" ident dest ; string url ]
  ]

let unzip zip =
  workflow ~descr:"utils.unzip" [
    cmd "unzip" [ opt "-d" ident dest ; dep zip ]
  ]

let gunzip gz =
  workflow ~descr:"utils.gunzip" [
    cmd "gunzip" [ opt "-c" dep gz ] ~stdout:dest
  ]

let tar_xfz tgz =
  workflow ~descr:"utils.tar_xfz" [
    mkdir_p dest ;
    cmd "tar" [ string "xfz" ; dep tgz ; opt "-C" ident dest ] ;
  ]

let crlf2lf f =
  workflow ~descr:"utils.crlf2lf" [
    cmd "tr" [ opt "-d" string "'\r'"] ~stdin:(dep f) ~stdout:dest
  ]
