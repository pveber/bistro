type 'a workflow = 'a Bistro.Workflow.t

class type ['a,'b] file = object
  method format : 'a
  method encoding : [< `text | `binary] as 'b
end

type 'a directory = [`directory of 'a]
type package = [`package] directory
type 'a zip = ([`zip of 'a], [`binary]) file
type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
type 'a tgz = ([`tgz of 'a],[`binary]) file
type pdf = ([`pdf],[`text]) file
type html = ([`html], [`text]) file
type bash_script = ([`bash_script], [`text]) file

type png = ([`png],[`binary]) file
type svg = ([`png],[`text]) file

class type ['a] tabular = object ('a)
  constraint 'a = < header : 'b ; sep : 'c ; comment : 'd ; .. >
  inherit [[`tabular], [`text]] file
  method header : 'b
  method sep : 'c
  method comment : 'd
end

class type ['a] tsv = object
  inherit [ < sep : [`tab] ; comment : [`sharp] ; .. > as 'a ] tabular
end


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
