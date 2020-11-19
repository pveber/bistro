open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"meme" ~tag:"4.11.2" () ]

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw (* ?np:threads *) fa =
  Workflow.shell ~descr:"meme-chip" ~img (* ?np:threads *) [
    cmd "meme-chip" [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      (* opt "-meme-p" Fn.id np ; *)(* this is disabled due to mpirun refusing to run as root under docker *)
      opt "--oc" Fn.id dest ;
      dep fa ;
    ]
  ]

let string_of_alphabet = function
  | `dna -> "dna"
  | `rna -> "rna"
  | `protein -> "protein"

let meme_alphabet_opt x =
  string ("-" ^ string_of_alphabet x)

let meme ?nmotifs ?minw ?maxw ?revcomp ?maxsize ?alphabet (* ?threads *) fa =
  Workflow.shell ~descr:"meme" ~img (* ?np:threads *) [
    cmd "meme" [
      option (opt "-nmotifs" int) nmotifs ;
      option (opt "-minw" int) minw ;
      option (opt "-maxw" int) maxw ;
      option meme_alphabet_opt alphabet ;
      option (flag string "-revcomp") revcomp ;
      option (opt "-maxsize" int) maxsize ;
      (* opt "-p" Fn.id np ; *) (* this is disabled due to mpirun refusing to run as root under docker *)
      opt "-oc" Fn.id dest ;
      dep fa ;
    ]
  ]

let meme_logo dir ?(rc = false) n =
  Workflow.select dir [ sprintf "logo%s%d.png" (if rc then "" else "_rc") n ]

let fimo
    ?alpha ?bgfile ?max_stored_scores ?max_strand ?motif ?motif_pseudo
    ?no_qvalue ?norc ?parse_genomic_coord ?prior_dist ?psp
    ?qv_thresh ?thresh meme_motifs seqs =
  Bistro.Workflow.shell ~descr:"meme_suite.fimo" ~img [
    cmd "fimo" [
      option (opt "--alpha" float) alpha ;
      option (opt "--bgfile" dep) bgfile ;
      option (opt "--max-stored-scores" int) max_stored_scores ;
      option (flag string "--max-strand") max_strand ;
      option (opt "--motif" string) motif ;
      option (opt "--motif-pseudo" float) motif_pseudo ;
      option (flag string "--no-qvalue") no_qvalue ;
      option (flag string "--norc") norc ;
      option (flag string "--parse-genomic-coord") parse_genomic_coord ;
      option (opt "--prior-dist" dep) prior_dist ;
      option (opt "--psp" dep) psp ;
      option (flag string "--qv-thresh") qv_thresh ;
      option (opt "--thresh" float) thresh ;
      opt "--oc" Fn.id dest ;
      dep meme_motifs ;
      dep seqs ;
    ]
  ]
