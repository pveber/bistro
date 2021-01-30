open Bistro.Shell_dsl

let s = [%script{|
I <- {{int 42}}
O <- {{dest}}
|}]

let u title = [%include_script "test/template.md"]
