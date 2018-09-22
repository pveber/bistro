(* A minimal pipeline fetching the lambda phage genome. *)

open Bistro_pack

let url = "https://www.ncbi.nlm.nih.gov/sviewer/viewer.cgi?tool=portal&save=file&log$=seqview&db=nuccore&report=fasta&id=215104&extrafeat=null&conwithfeat=on"

let lambda_phage_sequence : text_file workflow = Bistro_unix.wget url

let repo = Repo.[
    item ["lambda_phage.fa"] lambda_phage_sequence ;
  ]

let () = Repo.build ~outdir:"res" repo

