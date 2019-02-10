open Bistro.Shell_dsl

let script =
  let insert = int 42 in
  [%script {|
I={{insert}} 
|}]
