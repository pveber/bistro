open Core.Std

(* Unix utilities *)
let wget url = Bistro_workflow.(
  make [
    L [S"wget" ; S"-O" ; D ; S url]
  ]
)

let unzip (zip : [`zip of [`dir of 'a]] Bistro_file.t) : 'a Bistro_dir.t = Bistro_workflow.(
  make [
    L [S"unzip" ; S"-d" ; D ; W zip]
  ]
)

let tar_xfz (tgz : [`tgz of [`dir of 'a]] Bistro_file.t) : 'a Bistro_dir.t = Bistro_workflow.(
  make [
    L [S"mkdir" ; S"-p" ; D] ;
    L [S"tar" ; S"xfz" ; W tgz ; S"-C" ; D]
  ]
)

(* Wikipedia extractor toolkit *)
let wet_archive : [`tgz of [`dir of unit]] Bistro_file.t = wget "http://www.polishmywriting.com/download/wikipedia2text_rsm_mods.tgz"

let wet_distribution = tar_xfz wet_archive


let stanford_parser_archive : [`zip of [`dir of [`stanford_parser_distribution]]] Bistro_file.t =
  wget "http://nlp.stanford.edu/software/stanford-parser-full-2013-11-12.zip"

let stanford_parser_distribution : [`dir of [`stanford_parser_distribution]] Bistro_workflow.t =
  unzip stanford_parser_archive

let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

(* let () = *)
(*   Lwt_unix.run (Bistro_concurrent.dryrun db (task 60)) *)

let logger_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream (Bistro_logger.to_strings logger))

let () =
  Lwt_unix.run (Bistro_concurrent.exec db logger (Bistro_concurrent.local_worker ~np:4 ~mem:(6 * 1024)) wet_distribution)

