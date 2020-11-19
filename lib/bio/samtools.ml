open Core_kernel
open Bistro
open Bistro.Shell_dsl

type 'a format = Bam | Sam

let bam = Bam
let sam = Sam


let img = [ docker_image ~account:"pveber" ~name:"samtools" ~tag:"1.3.1" () ]

let samtools subcmd ?stdout args =
  cmd "samtools" ?stdout (string subcmd :: args)

let sam_of_bam bam =
  Workflow.shell ~descr:"samtools.sam_of_bam" ~img [
    samtools "view" [
      opt "-o" Fn.id dest ;
      dep bam ;
    ]
  ]

let bam_of_sam sam =
  Workflow.shell ~descr:"samtools.bam_of_sam" ~img [
    samtools "view" [
      string "-S -b" ;
      opt "-o" Fn.id dest ;
      dep sam ;
    ]
  ]

let indexed_bam_of_sam sam =
  Workflow.shell ~descr:"samtools.indexed_bam_of_sam" ~img [
    mkdir_p dest ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> dest // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      dest // "temp.bam" ;
      opt "-o" Fn.id (dest // "reads.bam") ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
    rm_rf (dest // "temp.bam") ;
  ]

let sort ?on:order bam =
  Workflow.shell ~descr:"samtools.sort" ~img [
    samtools "sort" [
      option (fun o -> flag string "-n" Poly.(o = `name)) order ;
      dep bam ;
      opt "-o" Fn.id dest ;
    ] ;
  ]

let indexed_bam_of_bam bam =
  Workflow.shell ~descr:"samtools.indexed_bam_of_bam" ~img [
    mkdir_p dest ;
    samtools "sort" [
      dep bam ;
      opt "-o" Fn.id (dest // "reads.bam") ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
  ]

let indexed_bam_to_bam x = Workflow.select x ["reads.bam"]

let output_format_expr = function
  | Bam -> string "-b"
  | Sam -> string ""


let view ~output (* ?_1 ?u *) ?h ?_H (* ?c ?_L *) ?q (* ?m ?f ?_F ?_B ?s *) file =
  Workflow.shell ~descr:"samtools.view" ~img [
    cmd "samtools view" [
      output_format_expr output ;
      (* option (flag string "-1") _1 ; *)
      (* option (flag string "-u") u ; *)
      option (flag string "-h") h ;
      option (flag string "-H") _H ;
      (* option (flag string "-c") c ; *)
      (* option (opt "-L" dep) _L ; *)
      option (opt "-q" int) q ;
      (* option (opt "-m" int) m ; *)
      (* option (opt "-f" int) f ; *)
      (* option (opt "-F" int) _F ; *)
      (* option (flag string "-B") _B ; *)
      (* option (opt "-s" float) s ; *)
      dep file ;
      opt "-o" Fn.id dest ;
    ]
  ]

let faidx fa =
  Workflow.shell ~descr:"samtools.faidx" ~img [
    mkdir_p dest ;
    cmd "cp" [ dep fa ; dest // "sequences.fa" ] ;
    samtools "faidx" [ dest // "sequences.fa" ] ;
  ]

let fasta_of_indexed_fasta dir = Workflow.select dir ["sequences.fa"]

let flagstat sam_or_bam =
  Workflow.shell ~descr:"samtools.flagstat" ~img [
    samtools "flagstat" ~stdout:dest [
      dep sam_or_bam ;
    ]
  ]

let rmdup ?single_end indexed_bam =
  Workflow.shell ~descr:"samtools.rmdup" ~img [
    samtools "rmdup" [
      option (flag string "-s") single_end ;
      dep (indexed_bam_to_bam indexed_bam) ;
      dest ;
    ]
  ]
