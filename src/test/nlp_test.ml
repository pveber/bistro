open Core.Std
open Workflow

let wget url = make [
  L [S"wget" ; S"-O" ; D ; S url]
]

let unzip (zip : [`zip] File.t) = Workflow.(
  make [
    L [S"unzip" ; S"-d" ; D ; W zip]
  ]
)

let stanford_parser_archive : [`zip] File.t =
  wget "http://nlp.stanford.edu/software/stanford-parser-full-2013-11-12.zip"

let stanford_parser_distribution : [`dir of [`stanford_parser_distribution]] Workflow.t =
  unzip stanford_parser_archive

let () = Sequential.exec (Db.make "_bistro") stanford_parser_distribution
