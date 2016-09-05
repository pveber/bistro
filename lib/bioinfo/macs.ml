(* open Core_kernel.Std *)
(* open Bistro_workflow.Defs *)

(* let package = Bistro_workflow.make <:script< *)
(* URL=https://github.com/downloads/taoliu/MACS/MACS-1.4.2-1.tar.gz *)
(* ARCHIVE=`basename ${URL}` *)
(* PACKAGE=macs *)

(* PREFIX=`readlink -f #DEST` *)
(* mkdir -p #TMP *)
(* cd #TMP *)

(* wget ${URL} || (echo "failed to fetch ${PACKAGE}" && exit 1) *)
(* tar xvfz ${ARCHIVE} *)
(* cd ${ARCHIVE%-1\.tar.gz} *)
(* python setup.py install --prefix ${PREFIX} || (echo "failed to install ${PACKAGE}" && exit 1) *)

(* >> *)

(* type gsize = [`hs | `mm | `ce | `dm | `gsize of int] *)

(* let run ~gsize ?tagsize ?bandwidth ~pvalue ?control chIP = *)
(*   let gsize = match gsize with *)
(*     | `hs -> "hs" *)
(*     | `mm -> "mm" *)
(*     | `dm -> "dm" *)
(*     | `ce -> "ce" *)
(*     | `gsize n -> Int.to_string n *)
(*   in *)
(*   Bistro_workflow.make <:script< *)
(* export PATH=#w:package#/bin:$PATH *)
(* export PYTHON_PATH=#w:package#/lib/python2.7/site-packages *)

(* mkdir -p #DEST *)
(* macs14 --name=#DEST/macs --gsize=#s:gsize# \ *)
(*                      #? ts <- tagsize#[--tsize=#i:ts#] \ *)
(*                      #? bw <- bandwidth#[--bw=#i:bw#] \ *)
(*                      --pvalue=#f:pvalue# \ *)
(*                      -t #w:chIP# \ *)
(*                      #? c <- control#[-c #w:c#] *)
(*   >> *)


(* module Xls = struct *)
(*   type 'a header = (string * (int * (int * (int * (int * (int * (float * (float * 'a)))))))) *)

(*   type 'a format = ('a header, [`yes], [`sharp]) tsv *)
(*   type without_fdr = unit format *)
(*   type with_fdr = (float * unit) format *)
(* end *)

(* type 'a output = [`macs_output of 'a] directory *)

(* module No_control = struct *)

(*   type workflow = Xls.without_fdr output Bistro_workflow.t *)

(*   let run ?tagsize ?bandwidth ~gsize ~pvalue chIP = *)
(*     run ~gsize ?tagsize ?bandwidth ~pvalue chIP *)

(* end *)

(* module With_control = struct *)

(*   type workflow = Xls.with_fdr output Bistro_workflow.t *)

(*   let run ?tagsize ?bandwidth ~gsize ~pvalue ~control chIP = *)
(*     run ~gsize ?tagsize ?bandwidth ~pvalue ~control chIP *)

(* end *)

(* let peaks mo = Bistro_workflow.select mo "macs_peaks.xls" *)

(* let bed mo = Bistro_workflow.select mo "macs_peaks.bed" *)

