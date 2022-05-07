open Core

let assembly_summary =
  Bistro_unix.wget Biotk.Ncbi_genome.refseq_assembly_summary_url

let fetch_assembly ~genome_id ~assembly_id =
  let genome_number =
    match String.lsplit2 ~on:'_' genome_id with
    | Some ("GCF", x) when String.length x >= 9 -> x
    | _ -> invalid_argf "fetch_assembly: invalid genome identifier %s" genome_id ()
  in
  let f pos = String.sub genome_number ~pos ~len:3 in
  let url =
    sprintf
      "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/%s/%s/%s/%s_%s/%s_%s_genomic.fna.gz"
      (f 0) (f 3) (f 6) genome_id assembly_id genome_id assembly_id
  in
  Bistro_unix.wget url
