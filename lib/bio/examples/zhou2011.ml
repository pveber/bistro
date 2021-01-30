(**
   Paper: https://www.ncbi.nlm.nih.gov/pubmed/21700227
   Datasets: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506
*)
open Core
open Biotk
open Bistro
open Bistro_bio
open Bistro_bio.Formats
open Bistro_utils

let np = 4

type chIP_sample = [
  | `ChIP_Pho4_noPi
  | `ChIP_Pho4_highPi
  | `ChIP_Cbf1_noPi
  | `ChIP_Mock_noPi
]
[@@deriving show, enumerate]

type input_sample = [
  | `Input_Pho4_noPi
  | `Input_Pho4_highPi
  | `Input_Cbf1_noPi
  | `Input_Mock_noPi
]
[@@deriving show, enumerate]

type sample = [
  | chIP_sample
  | input_sample
]
[@@deriving show, enumerate]

type factor = [
  | `Pho4
  | `Cbf1
  | `Mock
]
[@@deriving show, enumerate]

type condition = [
  | `noPi
  | `highPi
]
[@@deriving show, enumerate]

let factor = function
  | `ChIP_Pho4_highPi
  | `ChIP_Pho4_noPi -> `Pho4
  | `ChIP_Cbf1_highPi
  | `ChIP_Cbf1_noPi -> `Cbf1
  | `ChIP_Mock_highPi
  | `ChIP_Mock_noPi -> `Mock

let condition = function
  | `ChIP_Mock_highPi
  | `ChIP_Pho4_highPi
  | `ChIP_Cbf1_highPi -> `highPi
  | `ChIP_Pho4_noPi
  | `ChIP_Cbf1_noPi
  | `ChIP_Mock_noPi -> `noPi

let control_sample = function
  | `ChIP_Cbf1_noPi -> `Input_Cbf1_noPi
  | `ChIP_Pho4_noPi -> `Input_Pho4_noPi
  | `ChIP_Pho4_highPi -> `Input_Pho4_highPi
  | `ChIP_Mock_noPi -> `Input_Mock_noPi

let genome = Ucsc_gb.genome_sequence `sacCer2
let genome_2bit = Ucsc_gb.genome_2bit_sequence `sacCer2

let srr_id = function
  | `ChIP_Pho4_noPi -> [ "SRR217304" ; "SRR217305" ]
  | `ChIP_Pho4_highPi -> [ "SRR217306" ]
  | `ChIP_Cbf1_noPi -> [ "SRR217310" ]
  | `ChIP_Mock_noPi -> [ "SRR217312" ]
  | `Input_WT_noPi -> [ "SRR217324" ]
  | `Input_Pho4_noPi -> [ "SRR217319" ]
  | `Input_Pho4_highPi -> [ "SRR217320" ]
  | `Input_Cbf1_noPi -> [ "SRR217323" ]
  | `Input_Mock_noPi -> [ "SRR217324" ]

let fastq x =
  srr_id x
  |> List1.of_list_exn
  |> List1.map ~f:(fun id ->
    Sra_toolkit.(fastq_dump fastq_gz) (`id id)
    |> Fastq_sample.compressed_se
  )

let ecoli_genome : fasta file =
  Bistro_unix.wget "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/005/845/GCF_000005845.2_ASM584v2/GCF_000005845.2_ASM584v2_genomic.fna.gz"
  |> Bistro_unix.gunzip

let fastq_screen x =
  Fastq_screen.fastq_screen
    (List1.hd (fastq x))
    ["E_coli", ecoli_genome]

let bowtie_index = Bowtie.bowtie_build genome

let mapped_reads x =
  let Cons (fq, additional_samples) = fastq x in
  Bowtie.bowtie ~v:1 bowtie_index ~additional_samples fq

let mapped_reads_bam x =
  Samtools.indexed_bam_of_sam (mapped_reads x)

let tf_peaks ?qvalue treatment_sample =
  let control_sample = control_sample treatment_sample in
  let treatment = mapped_reads treatment_sample in
  let control = mapped_reads control_sample in
  Macs2.callpeak ~mfold:(1,100) ?qvalue Macs2.sam ~control:[ control ] [ treatment ]

let centered_tf_peaks ?qvalue ~radius treatment_sample =
  let summits = Macs2.peak_summits (tf_peaks ?qvalue treatment_sample) in
  let chrom_sizes = Ucsc_gb.fetchChromSizes `sacCer2 in
  Bedtools.(slop ~mode:(`both radius) bed summits chrom_sizes)

let best_macs_summits ?qvalue ~n sample =
  let summits = Macs2.peak_summits (tf_peaks ?qvalue sample) in
  let open Bistro.Shell_dsl in
  Bistro.Workflow.shell ~descr:"best_macs_summits" [
    pipe [
      cmd "sort" [
        string "-r -g -k 5" ;
        dep summits ;
      ] ;
      cmd "head" ~stdout:dest [
        opt "-n" int n ;
      ]
    ]
  ]

let best_peak_sequences ?(nseqs = Int.max_value) ?qvalue ~radius treatment_sample =
  let summits = best_macs_summits ?qvalue ~n:nseqs treatment_sample in
  let chrom_sizes = Ucsc_gb.fetchChromSizes `sacCer2 in
  let regions = Bedtools.(slop ~mode:(`both radius) bed summits chrom_sizes) in
  Ucsc_gb.twoBitToFa genome_2bit (Bed.keep4 regions)

let meme ?(nseqs = 500) treatment_sample =
  best_peak_sequences ~nseqs ~qvalue:1e-10 ~radius:50 treatment_sample
  |> Meme_suite.meme ~nmotifs:3 ~minw:5 ~maxw:8 ~revcomp:true ~alphabet:`dna ~maxsize:1_000_000

let meme_motifs treatment_sample =
  Bistro.Workflow.select (meme treatment_sample) ["meme.txt"]

let meme_chip treatment_sample =
  best_peak_sequences ~qvalue:1e-10 ~radius:50 treatment_sample
  |> Meme_suite.meme_chip
    ~meme_nmotifs:3 ~meme_minw:5 ~meme_maxw:8

let chipqc =
  let samples = List.map all_of_chIP_sample ~f:(fun x -> {
        ChIPQC.id = show_chIP_sample x ;
        tissue = "yeast" ;
        factor = show_factor (factor x) ;
        replicate = "1" ;
        bam = mapped_reads_bam x ;
        peaks = Macs2.narrow_peaks (tf_peaks x) ;
      })
  in
  ChIPQC.run samples

let occdist_vs_peak_score treatment_sample : svg file =
  let open Bistro.Shell_dsl in
  let peaks = centered_tf_peaks ~radius:500 treatment_sample in
  let sequences = Ucsc_gb.twoBitToFa genome_2bit (Bed.keep4 peaks) in
  let occ = dep @@ Meme_suite.fimo (meme_motifs treatment_sample) sequences in
  let peaks = dep peaks in
  let script = [%script {|
occ <- read.table("{{occ}}/fimo.txt", sep="\t",header=T, comment.char="")
motifs <- sort(unique(occ$X.pattern.name))
peaks <- read.table("{{peaks}}", sep="\t", col.names=c("chr","start","end","id","score"))
peaks <- peaks[order(peaks$score, decreasing=T),]
svg("{{dest}}", height=10)
par(mfrow=c(length(motifs), 2))
for (m in motifs) {
  closest_occ <- sapply(peaks$id, function(p) {
      o <- occ[occ$X.pattern.name == m & occ$sequence.name == as.character(p), ]
      pos <- c(1000, (o$start + o$start + 1) / 2)
      pos[which.min(abs(pos - 500))]
    })
  plot(peaks$score, closest_occ, main=sprintf("motif %d",m),xlab="Peak score",ylab="Closest occ")
  close_occ <- abs(closest_occ - 500) < 100
  print(summary(close_occ))
  df <- data.frame(score = peaks$score, close_occ = close_occ)
  g <- glm(close_occ ~ score,df, family="binomial")
  plot(peaks$score, close_occ, xlab="Peak score", ylab="Close occ prob")
  lines(peaks$score, predict(g,type="resp"))
}
dev.off()
|}]
  in
  Bistro.Workflow.shell ~descr:"occdist_vs_peak_rank" [
    cmd "Rscript" [ file_dump script ]
  ]

let report =
  [%include_script "lib/bio/examples/zhou2011.md"]
  |> Report.Md.to_html

let repo = Repo.[
    item [ "report.html" ] report ;
    (* item [ "macs2" ; "Pho4" ; "noPi" ] (tf_peaks `ChIP_Pho4_noPi) ;
     * item [ "meme" ; "Pho4" ; "noPi" ] (meme `ChIP_Pho4_noPi) ;
     * item [ "meme_chip" ; "Pho4" ; "noPi" ] (meme_chip `ChIP_Pho4_noPi) ;
     * item [ "chIP-QC" ; "Pho4" ; "noPi" ] chipqc ;
     * item [ "fastq-screen" ; "Pho4" ; "highPi" ] (fastq_screen `ChIP_Pho4_highPi) ; *)
  ]

let run () =
  Repo.build_main
    ~np ~mem:(`GB 4)
    ~outdir:"res"
    ~loggers:[ Console_logger.create () ]
    repo
