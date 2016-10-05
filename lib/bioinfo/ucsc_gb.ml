open Core_kernel.Std
open Bistro
open Bistro.Std
open Bistro.EDSL
open Defs

type genome = [ `dm3 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]

let string_of_genome = function
| `dm3 -> "dm3"
| `hg18 -> "hg18"
| `hg19 -> "hg19"
| `hg38 -> "hg38"
| `mm8 -> "mm8"
| `mm9 -> "mm9"
| `mm10 -> "mm10"
| `sacCer2 -> "sacCer2"


type twobit = ([`twobit], [`binary]) file

class type chrom_sizes = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : int
end

type bigBed = ([`bigBed], [`binary]) file

type bedGraph = ([`bedGraph], [`text]) file

type wig = ([`wig], [`text]) file

type bigWig = ([`bigWig], [`binary]) file

let env = docker_image ~account:"pveber" ~name:"ucsc-kent" ~tag:"330" ()


(** {5 Dealing with genome sequences} *)
    
let chromosome_sequences org =
  let org = string_of_genome org in
  workflow ~descr:(sprintf "ucsc_gb.chromosome_sequence(%s)" org) [
    mkdir_p dest ;
    cd dest ;
    wget (sprintf "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org) () ;
    cmd "gunzip" [ string "*.gz" ]
  ]

let genome_sequence org =
  let chr_seqs = chromosome_sequences org in
  workflow ~descr:"ucsc_gb.genome_sequence" [
    cmd "bash" [
      opt "-c" string "'shopt -s nullglob ; cat $0/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > $1'" ;
      dep chr_seqs ;
      dest
    ]
  ]

(* UGLY hack due to twoBitToFa: this tool requires that the 2bit
   sequence should be put in a file with extension 2bit. So I'm forced
   to create first a directory and then to select the unique file in it...*)
let genome_2bit_sequence_dir org =
  let org = string_of_genome org in
  workflow ~descr:(sprintf "ucsc_gb.2bit_sequence(%s)" org) [
    mkdir dest ;
    cd dest ;
    wget (sprintf "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/bigZips/%s.2bit" org org) () ;
  ]

let genome_2bit_sequence org =
  (genome_2bit_sequence_dir org) / selector [ (string_of_genome org) ^ ".2bit" ]

(* (\* let wg_encode_crg_mappability n org = *\) *)
(* (\*   let url = sp "ftp://hgdownload.cse.ucsc.edu/gbdb/%s/bbi/wgEncodeCrgMapabilityAlign%dmer.bigWig" (string_of_genome org) n in *\) *)
(* (\*   Guizmin_unix.wget url *\) *)

(* (\* let wg_encode_crg_mappability_36 org = wg_encode_crg_mappability 36 org *\) *)
(* (\* let wg_encode_crg_mappability_40 org = wg_encode_crg_mappability 40 org *\) *)
(* (\* let wg_encode_crg_mappability_50 org = wg_encode_crg_mappability 50 org *\) *)
(* (\* let wg_encode_crg_mappability_75 org = wg_encode_crg_mappability 75 org *\) *)
(* (\* let wg_encode_crg_mappability_100 org = wg_encode_crg_mappability 100 org *\) *)

let twoBitToFa bed twobits =
  workflow ~descr:"ucsc_gb.twoBitToFa" [
    cmd ~env "twoBitToFa" [
      opt' "-bed" dep bed ;
      dep twobits ;
      dest
    ]
  ]

(* let fasta_of_bed org bed = *)
(*   twoBitToFa bed (genome_2bit_sequence org) *)

(* (\*   f2 *\) *)
(* (\*     "guizmin.bioinfo.ucsc.fasta_of_bed[1]" [] *\) *)
(* (\*     seq2b bed *\) *)
(* (\*     (fun env (File seq2b) (File bed) path -> *\) *)
(* (\*       twoBitToFa ~positions:(`bed bed) ~seq2b ~fa:path) *\) *)

(* (\* let fetch_sequences (File seq2b) locations = *\) *)
(* (\*   let open Core_kernel.Std in *\) *)
(* (\*   Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".seqList" (fun seqList -> *\) *)
(* (\*     Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".fa" (fun fa -> *\) *)
(* (\*       (\\* Write locations to a file *\\) *\) *)
(* (\*       List.map locations Fungen.Location.to_string *\) *)
(* (\*       |> Out_channel.write_lines seqList ; *\) *)

(* (\*       (\\* run twoBitToFa *\\) *\) *)
(* (\*       twoBitToFa ~positions:(`seqList seqList) ~seq2b ~fa ; *\) *)

(* (\*       (\\* Parse the fasta file *\\) *\) *)
(* (\*       In_channel.with_file fa ~f:(fun ic -> *\) *)
(* (\*         Biocaml.fasta (in_channel_to_char_seq_item_stream_exn ic) *\) *)
(* (\*         /@ (fun x -> x.Biocaml.fasta sequence) *\) *)
(* (\*         |> Stream.to_list *\) *)
(* (\*       ) *\) *)
(* (\*     ) *\) *)
(* (\*   ) *\) *)

(** {5 Chromosome size and clipping} *)

let fetchChromSizes org =
  workflow ~descr:"ucsc_gb.fetchChromSizes" [
    cmd "fetchChromSizes" ~env ~stdout:dest [
      string (string_of_genome org) ;
    ]
  ]

let bedClip org bed =
  workflow ~descr:"ucsc_gb.bedClip" [
    cmd "bedClip -verbose=2" ~env [
      dep bed ;
      dep org ;
      dest ;
    ]
  ]


(* (\* let bedClip org bed = *\) *)
(* (\*   let chrom_info = chrom_info org in *\) *)
(* (\*   f2 *\) *)
(* (\*     "guizmin.bioinfo.ucsc.bedClip[r1]" [] *\) *)
(* (\*     chrom_info bed *\) *)
(* (\*     (fun env (File chrom_info) (File bed) path -> *\) *)
(* (\*        env.sh "bedClip -verbose=2 %s %s %s" bed chrom_info path) *\) *)



(** {5 Conversion between annotation file formats} *)

(* (\* let wig_of_bigWig bigWig = *\) *)
(* (\*   f1 *\) *)
(* (\*     "guizmin.bioinfo.ucsc.wig_of_bigWig[r1]" [] *\) *)
(* (\*     bigWig *\) *)
(* (\*     ( *\) *)
(* (\*       fun env (File bigWig) path -> *\) *)
(* (\* 	env.bash [ *\) *)
(* (\*           sp "bigWigToWig %s %s" bigWig path *\) *)
(* (\* 	] *\) *)
(* (\*     ) *\) *)

(* (\* let bigWig_of_wig ?(clip = false) org wig = *\) *)
(* (\*   let chrom_info = chrom_info org in *\) *)
(* (\*   f2 *\) *)
(* (\*     "guizmin.bioinfo.ucsc.bigWig_of_wig[r1]" [] *\) *)
(* (\*     chrom_info wig *\) *)
(* (\*     (fun env (File chrom_info) (File wig) path -> *\) *)
(* (\*       let clip = if clip then "-clip" else "" in *\) *)
(* (\*       env.sh "wigToBigWig %s %s %s %s" clip wig chrom_info path) *\) *)

let bedGraphToBigWig org bg =
  let tmp = seq [ tmp ; string "/sorted.bedGraph" ] in
  workflow ~descr:"bedGraphToBigWig" [
    cmd "sort" ~stdout:tmp [
      string "-k1,1" ;
      string "-k2,2n" ;
      dep bg ;
    ] ;
    cmd "bedGraphToBigWig" ~env [
      tmp ;
      dep (fetchChromSizes org) ;
      dest ;
    ]
  ]

let bedToBigBed_command org bed =
  let tmp = seq [ tmp ; string "/sorted.bed" ] in
  let sort =
    cmd "sort" ~stdout:tmp [
      string "-k1,1" ;
      string "-k2,2n" ;
      dep bed ;
    ] in
  let bedToBigBed =
    cmd "bedToBigBed" ~env [
      tmp ;
      dep (fetchChromSizes org) ;
      dest ;
    ]
  in
  [ sort ; bedToBigBed ]

let bedToBigBed org =
  let f bed =
    workflow
      ~descr:"ucsc_gb.bedToBigBed"
      (bedToBigBed_command org bed)
  in
  function
  | `bed3 bed -> f bed
  | `bed5 bed -> f bed

(* implements the following algorithm
   if bed is empty
   then touch target
   else bedToBigBed (sort bed)
*)
let bedToBigBed_failsafe org =
  let f bed =
    let test = cmd "test" [ string "! -s" ; dep bed ] in
    let touch = cmd "touch" [ dest ] in
    let cmd = or_list [
        and_list [ test ; touch ] ;
        and_list (bedToBigBed_command org bed) ;
      ] in
    workflow [ cmd ]
  in
  function
  | `bed3 bed -> f bed
  | `bed5 bed -> f bed


(* (\* module Lift_over = struct *\) *)
(* (\*   open Printf *\) *)
(* (\*   open Core_kernel.Std *\) *)
(* (\*   open CFStream *\) *)
(* (\*   open Stream.Infix *\) *)
(* (\*   open Fungen *\) *)

(* (\*   type chain_file = [`lift_over_chain] file *\) *)

(* (\*   let chain_file ~org_from ~org_to = *\) *)
(* (\*     let org_from = string_of_genome org_from *\) *)
(* (\*     and org_to = string_of_genome org_to in *\) *)
(* (\*     let url = *\) *)
(* (\*       sprintf *\) *)
(* (\*         "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/liftOver/%sTo%s.over.chain.gz" *\) *)
(* (\*         org_from org_from (String.capitalize org_to) *\) *)
(* (\*     in *\) *)
(* (\*     Guizmin_unix.(gunzip (wget url)) *\) *)

(* (\*   let create_input_file xs = *\) *)
(* (\*     let fn = Filename.temp_file "gzm" ".locations" in *\) *)
(* (\*     xs /@ Location.to_string *\) *)
(* (\*     |! lines_to_file fn ; *\) *)
(* (\*     fn *\) *)

(* (\*   let liftOver_cmd ~output ~chain_file ~old_locs ~new_locs ~unmapped_locs = *\) *)
(* (\*     let string_of_output = function *\) *)
(* (\*     | `bed -> "" *\) *)
(* (\*     | `position -> "-positions" *\) *)
(* (\*     in *\) *)
(* (\*     sprintf *\) *)
(* (\*       "liftOver %s %s %s %s %s" *\) *)
(* (\*       (string_of_output output) *\) *)
(* (\*       old_locs chain_file new_locs unmapped_locs *\) *)

(* (\*   let conversion (File chain_file) xs = *\) *)
(* (\*     let old_locs_fn = create_input_file xs in *\) *)
(* (\*     let new_locs_fn = Filename.temp_file "gzm" ".locations" *\) *)
(* (\*     and unmapped_locs_fn = (\\* unmapped locations *\\) *\) *)
(* (\*       Filename.temp_file "gzm" ".locations" in *\) *)
(* (\*     let cmd = liftOver_cmd ~output:`position ~chain_file ~old_locs:old_locs_fn ~new_locs:new_locs_fn ~unmapped_locs:unmapped_locs_fn in *\) *)
(* (\*     sh "%s 2> /dev/null" cmd ; *\) *)
(* (\*     let new_locs = *\) *)
(* (\*       List.map (lines_of_file new_locs_fn) ~f:Location.of_string *\) *)
(* (\*     and unmp_locs = *\) *)
(* (\*       List.map (lines_of_file unmapped_locs_fn) ~f:Location.of_string *\) *)
(* (\*     in *\) *)
(* (\*     sh "rm -f %s %s %s liftOver_*" old_locs_fn new_locs_fn unmapped_locs_fn ; *\) *)
(* (\*     new_locs, unmp_locs *\) *)

(* (\*   let bed_conversion ~org_from ~org_to bed = *\) *)
(* (\*     let chain_file = chain_file ~org_from ~org_to in *\) *)
(* (\*     d2 *\) *)
(* (\*       "guizmin.bioinfo.ucsc.bed_conversion[r2]" [] *\) *)
(* (\*       chain_file bed *\) *)
(* (\*       (fun env (File chain_file) (File bed) path -> *\) *)
(* (\*         env.sh "mkdir -p %s" path ; *\) *)
(* (\*         env.sh "%s" ( *\) *)
(* (\*           liftOver_cmd *\) *)
(* (\*             ~output:`bed *\) *)
(* (\*             ~chain_file *\) *)
(* (\*             ~old_locs:bed *\) *)
(* (\*             ~new_locs:(path ^ "/mapped.bed") *\) *)
(* (\*             ~unmapped_locs:(path ^ "/unmapped.bed") *\) *)
(* (\*         )) *\) *)

(* (\*   let mapped x = select x "mapped.bed" *\) *)
(* (\*   let unmapped x = select x "unmapped.bed" *\) *)
(* (\* end *\) *)
