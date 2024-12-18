open Bistro.Shell_dsl

let _s = [%script{|
I <- {{int 42}}
O <- {{dest}}
|}]

let _u title = [%include_script "template.md"]
