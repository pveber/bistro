open Bistro.Shell_dsl

let s = [%script{|
I <- {{int 42}}
O <- {{dest}}
|}]
