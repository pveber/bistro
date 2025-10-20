open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"busco" ~tag:"3.0.2" () ]

type db = [
  | `bacteria
  | `proteobacteria
  | `rhizobiales
  | `betaproteobacteria
  | `gammaproteobacteria
  | `enterobacteriales
  | `deltaepsilonsub
  | `actinobacteria
  | `cyanobacteria
  | `firmicutes
  | `clostridia
  | `lactobacillales
  | `bacillales
  | `bacteroidetes
  | `spirochaetes
  | `tenericutes
  | `eukaryota
  | `fungi
  | `microsporidia
  | `dikarya
  | `ascomycota
  | `pezizomycotina
  | `eurotiomycetes
  | `sordariomyceta
  | `saccharomyceta
  | `saccharomycetales
  | `basidiomycota
  | `metazoa
  | `nematoda
  | `arthropoda
  | `insecta
  | `endopterygota
  | `hymenoptera
  | `diptera
  | `vertebrata
  | `actinopterygii
  | `tetrapoda
  | `aves
  | `mammalia
  | `euarchontoglires
  | `laurasiatheria
  | `embryophyta
  | `protists_ensembl
  | `alveolata_stramenophiles_ensembl
]

let string_of_db = function
  | `bacteria -> "bacteria"
  | `proteobacteria -> "proteobacteria"
  | `rhizobiales -> "rhizobiales"
  | `betaproteobacteria -> "betaproteobacteria"
  | `gammaproteobacteria -> "gammaproteobacteria"
  | `enterobacteriales -> "enterobacteriales"
  | `deltaepsilonsub -> "deltaepsilonsub"
  | `actinobacteria -> "actinobacteria"
  | `cyanobacteria -> "cyanobacteria"
  | `firmicutes -> "firmicutes"
  | `clostridia -> "clostridia"
  | `lactobacillales -> "lactobacillales"
  | `bacillales -> "bacillales"
  | `bacteroidetes -> "bacteroidetes"
  | `spirochaetes -> "spirochaetes"
  | `tenericutes -> "tenericutes"
  | `eukaryota -> "eukaryota"
  | `fungi -> "fungi"
  | `microsporidia -> "microsporidia"
  | `dikarya -> "dikarya"
  | `ascomycota -> "ascomycota"
  | `pezizomycotina -> "pezizomycotina"
  | `eurotiomycetes -> "eurotiomycetes"
  | `sordariomyceta -> "sordariomyceta"
  | `saccharomyceta -> "saccharomyceta"
  | `saccharomycetales -> "saccharomycetales"
  | `basidiomycota -> "basidiomycota"
  | `metazoa -> "metazoa"
  | `nematoda -> "nematoda"
  | `arthropoda -> "arthropoda"
  | `insecta -> "insecta"
  | `endopterygota -> "endopterygota"
  | `hymenoptera -> "hymenoptera"
  | `diptera -> "diptera"
  | `vertebrata -> "vertebrata"
  | `actinopterygii -> "actinopterygii"
  | `tetrapoda -> "tetrapoda"
  | `aves -> "aves"
  | `mammalia -> "mammalia"
  | `euarchontoglires -> "euarchontoglires"
  | `laurasiatheria -> "laurasiatheria"
  | `embryophyta -> "embryophyta"
  | `protists_ensembl -> "protists_ensembl"
  | `alveolata_stramenophiles_ensembl -> "alveolata_stramenophiles_ensembl"

let file_of_db = function
  | `protists_ensembl
  | `alveolata_stramenophiles_ensembl as x ->
    string_of_db x
  | x -> string_of_db x ^ "_odb9"

let fetch_db (db : db) =
  let url =
    sprintf
      "http://busco.ezlab.org/v2/datasets/%s.tar.gz"
      (file_of_db db)
  in
  Bistro_unix.wget url
  |> Bistro_unix.tar_xfz ~strip_components:1


let config_file = seq ~sep:"\n" [
    string "[busco]" ;
    seq ~sep:" " [ string "out_path =" ; dest ] ;
    string {|
[tblastn]
path = /usr/bin/

[makeblastdb]
path = /usr/bin/

[augustus]
path = /home/osboxes/BUSCOVM/augustus/augustus-3.2.2/bin/

[etraining]
path = /home/osboxes/BUSCOVM/augustus/augustus-3.2.2/bin/

[gff2gbSmallDNA.pl]
path = /home/osboxes/BUSCOVM/augustus/augustus-3.2.2/scripts/
[new_species.pl]
path = /home/osboxes/BUSCOVM/augustus/augustus-3.2.2/scripts/
[optimize_augustus.pl]
path = /home/osboxes/BUSCOVM/augustus/augustus-3.2.2/scripts/

[hmmsearch]
path = /usr/local/bin/

[Rscript]
path = /usr/bin/
|}
  ]

let string_of_mode = function
  | `genome -> "genome"
  | `transcriptome -> "transcriptome"
  | `proteins -> "proteins"

let busco ?evalue ?limit ?tarzip ~threads ~mode ~db fa =
  Workflow.shell ~descr:"busco" ~img ~np:threads [
      and_list [
        cmd "" [ seq ~sep:"" [ string "export BUSCO_CONFIG_FILE=" ; file_dump config_file ] ] ;
        cmd "run_BUSCO.py" [
          opt "--in" dep fa ;
          opt "--lineage" dep (fetch_db db) ;
          opt "--mode" (string_of_mode % string) mode ;
          opt "--out" string "busco" ;
          opt "--cpu" Fn.id np ;
          option (opt "--evalue" float) evalue ;
          option (opt "--limit" int) limit ;
          option (flag string "--tarzip") tarzip ;
        ]
      ]
  ]
