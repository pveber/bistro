open Core
open Bistro.Std
open Bistro.EDSL
open Bistro_bioinfo.Std

type db = [`blast_db] directory
let env = docker_image ~account:"pveber" ~name:"ncbi-blast" ~tag:"2.4.0" ()

let db_name = "db"

let fastadb fa dbtype = 
	workflow ~descr:"blast.makedb" [
		mkdir_p dest ; 
		cmd ~env "makeblastdb" [
			opt "-in" dep fa ;
			opt "-dbtype" ident dbtype ; 
			opt "-out" ident (dest // db_name) ;  	
		] ; 	
	]

(* Basic blastn*)

let blastn ?evalue ?word_size ?task ?gapopen ?gapextend ?penalty
?reward ?outfmt ?perc_identity ?qcov_hsp_perc ?max_hsps ?max_target_seqs ?(threads = 4) db query out_name = (*See blastn documentation to know what options are*)
	workflow ~descr:"blastn" ~np:threads [ 
		mkdir_p dest ; 
		cmd "blastn" ~env [
			opt "-db" ident (dep db // db_name) ; 
			opt "-query" dep query ; 
			opt "-out" ident (dest // out_name) ; 
			option (opt "-evalue" float) evalue ;
			option (opt "-word_size" int) word_size ;
			option (opt "-task" string) task ; 
			option (opt "-gapopen" int) gapopen ;
			option (opt "-gapextend" int) gapextend ; 
			option (opt "-penalty" int) penalty ; 
			option (opt "-reward" int) reward ; 
			option (opt "-outfmt" string) outfmt ; 
			option (opt "-perc_identity" float) perc_identity ; 
			option (opt "-qcov_hsp_perc" float) qcov_hsp_perc ; 
			option (opt "-max_hsps" int) max_hsps ; 
			option (opt "-max_target_seqs" int) max_target_seqs ; 
			opt "-num_threads" ident np ; 
		]
	]

let blastp ?evalue ?word_size ?task ?gapopen ?gapextend ?penalty
?reward ?outfmt ?perc_identity ?qcov_hsp_perc ?max_hsps ?max_target_seqs ?(threads = 4) db query out_name = (*See blastn documentation to know what options are*)
	workflow ~descr:"blastp" ~np:threads [ 
		mkdir_p dest ; 
		cmd "blastp" ~env [
			opt "-db" ident (dep db // db_name) ; 
			opt "-query" dep query ; 
			opt "-out" ident (dest // out_name) ; 
			option (opt "-evalue" float) evalue ;
			option (opt "-word_size" int) word_size ;
			option (opt "-task" string) task ; 
			option (opt "-gapopen" int) gapopen ;
			option (opt "-gapextend" int) gapextend ; 
			option (opt "-penalty" int) penalty ; 
			option (opt "-reward" int) reward ; 
			option (opt "-outfmt" string) outfmt ; 
			option (opt "-perc_identity" float) perc_identity ; 
			option (opt "-qcov_hsp_perc" float) qcov_hsp_perc ; 
			option (opt "-max_hsps" int) max_hsps ; 
			option (opt "-max_target_seqs" int) max_target_seqs ; 
			opt "-num_threads" ident np ; 
		]
	]	



