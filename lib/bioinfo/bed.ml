open Printf
open Bistro.EDSL

let keep ~n bed =
  if n < 1 then raise (Invalid_argument "Bed.keep") ;
  workflow ~descr:"bed.keep" [
    shcmd "cut" ~stdout:dest [
      string (sprintf "-f 1-%d" n) ;
      dep bed ;
    ]
  ]

let keep3 x = keep ~n:3 x

let keep4 x = keep ~n:4 x

let keep5 x = keep ~n:5 x
