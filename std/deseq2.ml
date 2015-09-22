open Core_kernel.Std
open Types
open Bistro.EDSL_sh

let r_library_package = Bistro.Workflow.make ~descr:"deseq2.package" [%sh{|
DEST={{ dest }}

set -e
mkdir -p $DEST
export R_LIBS_USER=$DEST
echo 'source("http://bioconductor.org/biocLite.R") ; biocLite("DESeq2")' | R --vanilla
|}]

(* let wrapper_package = Bistro.Workflow.make [%sh{| *)
(*     mkdir_p (dest // "bin") ; *)
(*     wget *)
(*       "https://raw.githubusercontent.com/pveber/compbio-scripts/master/deseq2-wrapper/0.0.1/deseq2-wrapper.R" *)
(*       ~dest:(dest // "bin/deseq2-wrapper.R") () ; *)
(*     cmd "chmod" [ *)
(*       string "u+x" ; *)
(*       (dest // "bin/deseq2-wrapper.R") *)
(*     ] *)
(*   ] |}] *)

(* type wrapper_output = [`deseq2_wrapper_output] directory *)

(* let wrapper factors samples = *)
(*   let factors = opt "--factors" (list string ~sep:",") factors in *)
(*   let samples = List.map samples ~f:(fun (factor_vals, counts) -> *)
(*       seq [ list string ~sep:"," factor_vals ; string "," ; dep counts ] *)
(*     ) *)
(*   in *)
(*   let outdir = opt "--outdir" ident dest in *)
(*   workflow [ *)
(*     cmd ~path:[wrapper_package] "deseq2-wrapper.R" (outdir :: factors :: samples) ; *)
(*   ] *)

(* let index_of_wrapper_output o = Workflow.extract o [ "index.html" ] *)
