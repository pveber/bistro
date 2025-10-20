# Bistro tutorial

## Getting started with OCaml

Let's have a first taste of the library by writing some simple
definitions in the OCaml interpreter (tip: use
[`utop`](https://github.com/diml/utop/) for a better experience):


## First steps


```ocaml
#require "bistro.bioinfo";;

open Bistro.Std;;
open Bistro_bioinfo.Std;;

let sample = Sra.fetch_srr "SRR217304";;
```

After loading the library, we open two modules that provide convenient
definitions. Then we define a value named `sample` by calling a
function from the `Sra` module. The value `sample` has type `sra
workflow`, meaning a workflow that produces files following the SRA
format defined by NCBI.

## Basic usage
