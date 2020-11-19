open Core_kernel
open Bistro
open Biotk
open Formats

type t =
  | Fq of fastq file SE_or_PE.t
  | Fq_gz of fastq gz file SE_or_PE.t

let is_single_end = function
  | Fq (Single_end _)
  | Fq_gz (Single_end _ ) -> true
  | Fq (Paired_end _)
  | Fq_gz (Paired_end _) -> false

let plain_se x = Fq (SE_or_PE.Single_end x)
let plain_pe x y = Fq (SE_or_PE.Paired_end (x, y))
let compressed_se x = Fq_gz (SE_or_PE.Single_end x)
let compressed_pe x y = Fq_gz (SE_or_PE.Paired_end (x, y))

let dep = function
  | Fq se_or_pe -> SE_or_PE.map se_or_pe ~f:Shell_dsl.dep
  | Fq_gz se_or_pe -> SE_or_PE.map se_or_pe ~f:Bistro_unix.Cmd.gzdep

let explode fq_samples =
  let fqs, fqs1, fqs2, fqs_gz, fqs1_gz, fqs2_gz =
    List.fold fq_samples ~init:([],[],[],[],[],[]) ~f:(fun (fqs, fqs1, fqs2, fqs_gz, fqs1_gz, fqs2_gz) x ->
        match x with
        | Fq (Single_end fq) -> (fq :: fqs, fqs1, fqs2, fqs_gz, fqs1_gz, fqs2_gz)
        | Fq (Paired_end (fq1, fq2)) -> (fqs, fq1 :: fqs1, fq2 :: fqs2, fqs_gz, fqs1_gz, fqs2_gz)
        | Fq_gz (Single_end fq) -> (fqs, fqs1, fqs2, fq :: fqs_gz, fqs1_gz, fqs2_gz)
        | Fq_gz(Paired_end (fq1, fq2)) -> (fqs, fqs1, fqs2, fqs_gz, fq1 :: fqs1_gz, fq2 :: fqs2_gz)
      )
  in
  (List.rev fqs, List.rev fqs1, List.rev fqs2),
  (List.rev fqs_gz, List.rev fqs1_gz, List.rev fqs2_gz)

type source =
  | Fastq_url of string SE_or_PE.t
  | Fastq_gz_url of string SE_or_PE.t
  | SRA_dataset of { srr_id : string ;
                     library_type : [`single_end | `paired_end] }

let unsafe_file_of_url url : 'a file =
  if String.is_prefix ~prefix:"http://" url || String.is_prefix ~prefix:"ftp://" url
  then Bistro_unix.wget url
  else Workflow.input url

let rec fastq_gz_of_source = function
  | Fastq_url _ as s ->
    SE_or_PE.map (fastq_of_source s) ~f:Bistro_unix.gzip
  | Fastq_gz_url uris ->
    SE_or_PE.map uris ~f:unsafe_file_of_url
  | SRA_dataset { srr_id ; library_type } ->
    match library_type with
    | `paired_end ->
      let r1, r2 = Sra_toolkit.(fastq_dump_pe fastq_gz) (`id srr_id) in
      Paired_end (r1, r2)
    | `single_end ->
      Single_end (Sra_toolkit.(fastq_dump fastq_gz) (`id srr_id))

and fastq_of_source = function
  | Fastq_url uris ->
    SE_or_PE.map uris ~f:unsafe_file_of_url
  | Fastq_gz_url _
  | SRA_dataset _ as s ->
    SE_or_PE.map ~f:Bistro_unix.gunzip (fastq_gz_of_source s)

let of_source s =
  match s with
  | Fastq_url _ -> Fq (fastq_of_source s)
  | SRA_dataset _
  | Fastq_gz_url _ -> Fq_gz (fastq_gz_of_source s)

module type Data = sig
  type t
  val source : t -> source List1.t
end

module Make(Data : Data) = struct
  let fastq_gz x = List1.map (Data.source x) ~f:fastq_gz_of_source
  let fastq x = List1.map (Data.source x) ~f:fastq_of_source

  let fastq_samples s = List1.map (Data.source s) ~f:of_source

  let fastqc x =
    List1.map (fastq_gz x) ~f:(SE_or_PE.map ~f:FastQC.fastqc_gz)
end
