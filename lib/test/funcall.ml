open Core
open Bistro.Std
open Bistro.EDSL
open Bistro_utils

let echo s =
  workflow [
    cmd "echo" ~stdout:dest [ string s ]
  ]

let double_implem fn =
  let s = In_channel.read_all fn in
  s ^ s

let double w =
  let open Bistro.EDSL' in
  value (
    pure "double" double_implem $ dep w
  )

let double2 w =
  let open Bistro.EDSL' in
  value (
    pure "double2" (fun s -> s ^ s)
    $ valdep w
  )

let main () =
  let open Bistro_app in
  let w = double2 (double (echo "42!")) in
  let app = pure (function Path p -> print_endline p) $ pureW w in
  run app

let command =
  Command.basic
    ~summary:"Tests function call tasks"
    Command.Spec.empty
    main
