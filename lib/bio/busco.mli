open Bistro

val img : container_image list

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

val busco :
  ?evalue:float ->
  ?limit:int ->
  ?tarzip:bool ->
  threads:int ->
  mode:[`genome | `transcriptome | `proteins] ->
  db:db ->
  fasta file ->
  [`busco] directory
