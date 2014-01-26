open Bistro_workflow
open Bistro_types
open Core.Std

(* {5 Unix utilities} *)

let wget url = Bistro_workflow.(
  make <:script< wget -O #{DEST} #s:url# >>
)

let unzip (zip : 'a directory zip workflow) : 'a directory workflow = Bistro_workflow.(
  make <:script<
    unzip -d #{DEST} #w:zip#
  >>
)

let tar_xfz (tgz : 'a directory tgz workflow) : 'a directory workflow = Bistro_workflow.(
  make <:script<
    mkdir -p #{DEST}
    tar xfz #w:tgz# -C #{DEST}
  >>
)

let wikipedia_query q : [`text] file workflow = make <:script<
  dig +short txt "#s:q#".wp.dg.cx | sed -e 's/.\{1\}//1' | sed -e 's/\./\.\\\\n/g' | head -n 1 > #{DEST}
>>

(* {5 Wikipedia extractor toolkit} *)
let wet_archive : [`wet_archive] directory tgz workflow =
  wget "http://www.polishmywriting.com/download/wikipedia2text_rsm_mods.tgz"

let wet_package : package workflow = make <:script<
  tar xfz #w:wet_archive# -C #{DEST}TMP
  mkdir -p #{DEST}/bin
  mv #{TMP}/wikipedia2text/* #{DEST}/bin
>>

(* {5 Stanford Parser} *)
let stanford_parser_archive : [`stanford_parser_distribution] directory zip workflow =
  wget "http://nlp.stanford.edu/software/stanford-parser-full-2013-11-12.zip"

let stanford_parser_package : package workflow = make <:script<
  unzip -d #{TMP} #w:stanford_parser_archive#
  mkdir -p #{DEST}
  mv #{TMP}/stanford-parser-*/* #{DEST}
  (cd #{DEST} && wget "http://chaoticity.com/software/DependenSee.2.0.5.jar")
  sed -i 's/penn,//g' #{DEST}/lexparser.sh
>>

let stanford_parser x : [`stanford_parser_typed_dependencies] file workflow = make <:script<
  export PATH=#w:stanford_parser_package#:$PATH
  lexparser.sh #w:x# > #{DEST}
>>

let dependensee (x : [`stanford_parser_typed_dependencies] file workflow) = make <:script<
  java -cp #w:stanford_parser_package#/DependenSee.2.0.5.jar:#w:stanford_parser_package#/stanford-parser.jar:#w:stanford_parser_package#/stanford-parser-3.3.0-models.jar com.chaoticity.dependensee.Main -t #w:x# #{DEST}
>>




let db = Bistro_db.make "_bistro"
let () = Bistro_db.setup db
let logger = Bistro_logger.make ()

(* let () = *)
(*   Lwt_unix.run (Bistro_concurrent.dryrun db (task 60)) *)

let logger_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream (Bistro_logger.to_strings logger))

let () =
  let goal = dependensee (stanford_parser (wikipedia_query "Gene")) in
  Lwt_unix.run (Bistro_concurrent.exec db logger (Bistro_concurrent.local_worker ~np:4 ~mem:(6 * 1024)) goal)
