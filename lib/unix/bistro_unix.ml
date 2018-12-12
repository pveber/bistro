open Bistro
open Bistro.Shell_dsl
open Printf

let ident x = x

module Cmd = struct
  let wget ?no_check_certificate ?user ?password ?dest url =
    cmd "wget" [
      option (flag string "--no-check-certificate") no_check_certificate ;
      option (opt "--user" string) user ;
      option (opt "--password" string) password ;
      option (opt "-O" ident) dest ;
      string_dep url ;
    ]
end

let wget_dyn ?descr_url ?no_check_certificate ?user ?password url =
  let info = match descr_url with None -> "" | Some i -> sprintf "(%s)" i in
  Workflow.shell ~descr:("unix.wget" ^ info) [
    Cmd.wget ?no_check_certificate ?user ?password ~dest url
  ]

let wget ?descr_url ?no_check_certificate ?user ?password url =
  wget_dyn ?descr_url ?no_check_certificate ?user ?password (Workflow.string url)

let unzip zip =
  Workflow.shell ~descr:"unix.unzip" [
    cmd "unzip" [ opt "-d" ident dest ; dep zip ]
  ]

let gunzip gz =
  Workflow.shell ~descr:"unix.gunzip" [
    cmd "gunzip" [ opt "-c" dep gz ] ~stdout:dest
  ]

let bunzip2 bz2 =
  Workflow.shell ~descr:"unix.bunzip2" [
    cmd "bunzip2" [ opt "-c" dep bz2 ] ~stdout:dest
  ]

let tar_xfz ?strip_components tgz =
  Workflow.shell ~descr:"unix.tar_xfz" [
    mkdir_p dest ;
    cmd "tar" [
      string "xfz" ;
      dep tgz ;
      opt "-C" ident dest ;
      option (opt "--strip-components" int) strip_components ;
    ] ;
  ]

let tar_xfj ?strip_components tgj =
  Workflow.shell ~descr:"unix.tar_xfj" [
    mkdir_p dest ;
    cmd "tar" [
      string "xfj" ;
      dep tgj ;
      opt "-C" ident dest ;
      option (opt "--strip-components" int) strip_components ;
    ] ;
  ]

let crlf2lf f =
  Workflow.shell ~descr:"unix.crlf2lf" [
    cmd "tr" [ opt "-d" string "'\r'"] ~stdin:(dep f) ~stdout:dest
  ]
