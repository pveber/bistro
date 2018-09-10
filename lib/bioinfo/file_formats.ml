open Bistro_base.File_formats

class type bam = object
  inherit binary_file
  method format : [`bam]
end

class type bed3 = object
  inherit tsv
  method header : [`none]
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

class type bed6 = object
  inherit bed5
  method f6 : [ `Plus | `Minus | `Unknown ]
end

class type fasta = object
  inherit text_file
  method format : [`fasta]
end

class type fastq = object
  inherit text_file
  method format : [`fastq]
end

class type sanger_fastq = object
  inherit fastq
  method phred_encoding : [`sanger]
end

class type gff = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : string
  method f3 : string
  method f4 : int
  method f5 : int
  method f6 : float
  method f7 : [`Plus | `Minus]
  method f8 : [`frame0 | `frame1 | `frame2]
  method f9 : string
end

class type gff2 = object
  inherit gff
  method version : [`v2]
end

class type gff3 = object
  inherit gff
  method version : [`v3]
end

type indexed_bam = [`indexed_bam] directory

class type sam = object
  inherit text_file
  method format : [`sam]
end

class type sra = object
  inherit binary_file
  method format : [`sra]
end

class type twobit = object
  method format : [`twobit]
  inherit binary_file
end

class type chrom_sizes = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : int
end

class type bigBed = object
  method format : [`bigBed]
  inherit binary_file
end

class type bedGraph = object
  inherit bed3
  method f4 : float
end

class type wig = object
  method format : [`wig]
  inherit text_file
end

class type bigWig = object
  method format : [`bigWig]
  inherit binary_file
end
