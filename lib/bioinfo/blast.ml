open Core
open Bistro.Std
open Bistro.EDSL
open Bistro_bioinfo.Std

type db = [`blast_db] directory
let env = docker_image ~account:"pveber" ~name:"ncbi-blast" ~tag:"2.4.0" ()

let db_name = "db"

let makedb ~dbtype:dbtype fa = 
	let args = match dbtype with
	 | `Nucl -> string "-dbtype nucl" 	
	 | `Prot -> string "-dbtype prot"
  	in 
	workflow ~descr:"blast.makedb" [
		cmd ~env "makeblastdb" [
			opt "-in" dep fa ;
			opt "-out" ident (dest // db_name); 
			args ; 	
		] ; 	
	]

(* Basic blastn*)

let results = "results.blast"
let blastp ?evalue ?(threads = 4) ?outfmt db query = workflow ~descr:"blastp_xml" ~np:threads [
	mkdir_p dest ; 
    cmd "blastp" ~env [
      opt "-db" dep db // db_name ; 
	  opt "-query" dep query ; 
	  opt "-out" ident (dest // results) ; 
	  option (opt "-evalue" float) evalue ;
	  option (opt "-outfmt" string) outfmt ; 
    ]
  ]  

let blast_align = selector ["results.blast"] 