# bistro: build and run distributed workflows

`bistro` is an [OCaml](http://ocaml.org) library to build and run
computations represented by a collection of interdependent scripts, as
is often found in applied research (especially computational
biology).

**Features**:
- build complex and composable workflows declaratively
- simple and lightweight wrapping of new components
- resume-on-failure: if something fails, fix it and the workflow will
  restart from where it stopped
- distributed workflow execution
- development-friendly: when a script is modified, bistro
  automatically finds out what needs to be recomputed
- automatic naming of generated files
- static typing: detect file format errors at compile time!

The library provides a datatype to represent scripts (including
metadata and dependencies), an engine to run workflows and a
standard library providing components for popular tools (although
mostly related to computational biology and unix for now).

Questions, suggestions or contributions are welcome, please file an
[issue](https://github.com/pveber/bistro/issues) as needed.

## Installation

I recommend installing `bistro` using
[opam](http://opam.ocaml.org/) (see
[installation instructions](http://opam.ocaml.org/doc/Install.html)). You
need a recent (at least 4.03.0) installation of OCaml. Once this is
done, simply type

```
opam install bistro
```

to install the library.

## Usage

Here is an example of how we could write a typical workflow for
ChIP-seq data:

```ocaml
open Bistro.Std;;
open Bistro_bioinfo.Std;;

let sample = Sra.fetch_srr "SRR217304"                  (* Fetch a sample from the SRA database *)
let sample_fq = Sra_toolkit.fastq_dump sample           (* Convert it to FASTQ format *)
let genome = Ucsc_gb.genome_sequence `sacCer2           (* Fetch a reference genome *)
let bowtie2_index = Bowtie2.bowtie2_build genome        (* Build a Bowtie2 index from it *)
let sample_sam =                                        (* Map the reads on the reference genome *)
  Bowtie2.bowtie2 bowtie2_index (`single_end [ sample_fq ])
let sample_bam =                                        (* Convert SAM file to BAM format *)
  Samtools.(indexed_bam_of_sam sample_sam / indexed_bam_to_bam)
let sample_peaks = Macs2.callpeak sample_bam            (* Call peaks on mapped reads *)

let () =                                                (** Actually run the pipeline *)
  let repo = Bistro_repo.[
    [ "peaks" ] %> sample_peaks 
    ]
  in
  Bistro_repo.build ~outdir:"res" repo
```
