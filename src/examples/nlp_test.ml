open Bistro_workflow
open Bistro_types
open Core.Std

(* Unix utilities *)
let wget url = Bistro_workflow.(
  make <:script<
    wget -O %@ %s:url%
  >>
)

let unzip (zip : 'a directory zip workflow) : 'a directory workflow = Bistro_workflow.(
  make [
    L [S"unzip -d " ; D ; S" " ; W zip]
  ]
)

let tar_xfz (tgz : 'a directory tgz workflow) : 'a directory workflow = Bistro_workflow.(
  make [
    L [S"mkdir -p " ; D] ;
    L [S"tar xfz " ; W tgz ; S" -C " ; D]
  ]
)

(* Wikipedia extractor toolkit *)
let wet_archive : [`wet_archive] tgz workflow = wget "http://www.polishmywriting.com/download/wikipedia2text_rsm_mods.tgz"

let wet_package : package workflow =
  make [
    L [S"mkdir -p " ; D ] ;
  ]
(* tar_xfz wet_archive*)


let stanford_parser_archive : [`stanford_parser_distribution] directory zip workflow =
  wget "http://nlp.stanford.edu/software/stanford-parser-full-2013-11-12.zip"

let stanford_parser_distribution : [`stanford_parser_distribution] directory workflow =
  unzip stanford_parser_archive

let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

(* let () = *)
(*   Lwt_unix.run (Bistro_concurrent.dryrun db (task 60)) *)

let logger_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream (Bistro_logger.to_strings logger))

let () =
  Lwt_unix.run (Bistro_concurrent.exec db logger (Bistro_concurrent.local_worker ~np:4 ~mem:(6 * 1024)) wet_package)
