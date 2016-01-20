open Bistro.Std

type bam = ([`bam],[`binary]) file

class type bed3 = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : int
  method f3 : int
end

class type bed4 = object
  inherit bed3
  method f4 : string
end

class type bed5 = object
  inherit bed4
  method f5 : int
end

type fasta = ([`fasta],[`text]) file

class type gff = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : string
  method f3 : string
  method f4 : int
  method f5 : int
  method f6 : float
  method f7 : string
  method f8 : string
  method f9 : string
end

type 'a fastq = ([`fastq of 'a], [`text]) file

type sam = ([`sam],[`text]) file

type sra = ([`sra], [`binary]) file
