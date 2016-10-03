open Bistro_std

let a = 1

let s = Workflow.make [%bistro.ocaml {|
let a = {{ int a }}

let f x = x + 1
let () = a + b



|}]


let s = Workflow.make [%bistro.sh {|
ls {{TMP}} > {{DEST}}

for f in `rien`; do
               rm -f $f
done
|}]

