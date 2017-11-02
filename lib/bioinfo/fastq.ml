open Bistro.Std
open Bistro.EDSL
open Defs

type _ format =
  | Sanger  : [`sanger] format
  | Solexa  : [`solexa] format
  | Phred64 : [`phred64] format

let to_sanger :
  type s. s format -> s fastq workflow -> [`sanger] fastq workflow
  = fun format fq ->
    match format with
    | Sanger -> fq
    | Solexa -> failwith "not implemented"
    | Phred64 -> failwith "not implemented"

let concat = function
  | [] -> raise (Invalid_argument "fastq concat: empty list")
  | x :: [] -> x
  | fqs ->
    workflow ~descr:"fastq.concat" [
      cmd "cat" ~stdout:dest [ list dep ~sep:" " fqs ]
    ]

let head n fq =
  workflow ~descr:"fastq.head" [
    cmd "head" ~stdout:dest [
      opt "-n" int (n * 4) ;
      dep fq ;
    ]
  ]
