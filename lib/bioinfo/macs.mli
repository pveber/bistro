open Bistro.Std(* open Bistro_workflow.Types *)

(* val package : package workflow *)

(* module Xls : sig *)
(*   type 'a header = (string * (int * (int * (int * (int * (int * (float * (float * 'a)))))))) *)

(*   type 'a format = ('a header, [`yes], [`sharp]) tsv *)
(*   type without_fdr = unit format *)
(*   type with_fdr = (float * unit) format *)
(* end *)

(* type 'a output = [`macs_output of 'a] directory *)
(* type gsize = [`hs | `mm | `ce | `dm | `gsize of int] *)

(* module No_control : sig *)
(*   type workflow = Xls.without_fdr output Bistro_workflow.t *)

(*   val run : *)
(*     ?tagsize:int -> ?bandwidth:int -> *)
(*     gsize:gsize -> pvalue:float -> *)
(*     bam workflow -> workflow *)
(* end *)

(* module With_control : sig *)
(*   type workflow = Xls.with_fdr output Bistro_workflow.t *)

(*   val run : *)
(*     ?tagsize:int -> ?bandwidth:int -> *)
(*     gsize:gsize -> *)
(*     pvalue:float -> *)
(*     control:bam workflow -> *)
(*     bam workflow -> workflow *)
(* end *)

(* val peaks : 'a output workflow -> 'a workflow *)

(* val bed : 'a output workflow -> Bed.bed3 workflow *)
