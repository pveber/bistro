open Core.Std

let wget url = Bistro_workflow.(
  make [
    L [S"wget" ; S"-O" ; D ; S url]
  ]
)

let unzip (zip : [`zip] Bistro_file.t) = Bistro_workflow.(
  make [
    L [S"unzip" ; S"-d" ; D ; W zip]
  ]
)

let stanford_parser_archive : [`zip] Bistro_file.t =
  wget "http://nlp.stanford.edu/software/stanford-parser-full-2013-11-12.zip"

let stanford_parser_distribution : [`dir of [`stanford_parser_distribution]] Bistro_workflow.t =
  unzip stanford_parser_archive

let () = Bistro_sequential.exec (Bistro_db.make "_bistro") stanford_parser_distribution
