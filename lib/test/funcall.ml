open Core.Std
open Bistro.Std
open Bistro.EDSL

let echo s =
  workflow [
    cmd "echo" ~stdout:dest [ string s ]
  ]

let double_implem fn =
  let s = In_channel.read_all fn in
  s ^ s

let double w =
  let open E in
  value (
    primitive "double" double_implem $ dep w
  )

let main () =
  let open Bistro_app in
  let w = double (echo "42!") in
  let app = pure ignore $ pureW w in
  run app

let command =
  Command.basic
    ~summary:"Tests function call tasks"
    Command.Spec.empty
    main
