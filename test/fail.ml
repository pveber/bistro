open Bistro.Std
open Bistro

type failure

let fail1 : failure workflow = Workflow.make [%sh{|
false
|}]

let fail2 : failure workflow = Workflow.make [%sh{|
echo
|}]


let () =
  Bistro_app.(
    simple [
      [ "output" ; "fail1" ] %> fail1 ;
      [ "output" ; "fail2" ] %> fail2 ;
    ]
  )
