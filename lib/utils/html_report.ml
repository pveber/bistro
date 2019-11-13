open Core
open Bistro

module H = Tyxml.Html

type cell =
  | Text of string
  | Section of string
  | Subsection of string
  | Pdf of pdf file
  | Svg of svg file
  | Png of png file

type t = {
  title : string ;
  cells : cell list ;
}

let make ~title cells = { title ; cells }

let text s = Text s

let pdf x = Pdf x
let svg x = Svg x
let png x = Png x
let section x = Section x
let subsection x = Subsection x

let svg_of_pdf (pdf : pdf file) : svg file =
  Workflow.shell ~descr:"notebook.convert" Shell_dsl.[
    cmd "convert" [
      string "-append" ;
      seq ~sep:":" [string "pdf" ; dep pdf] ;
      seq ~sep:":" [string "svg" ; dest] ;
    ]
  ]

let picture  ?(alt = "") format path =
  let format = match format with
    | `svg -> "svg+xml"
    | `png -> "png"
  in
  let contents =
    In_channel.read_all path
    |> Base64.encode_exn
  in
  H.img
    ~src:(Printf.sprintf "data:image/%s;base64,%s" format contents)
    ~alt ()

let render_cell = function
  | Text str -> [%workflow_expr H.p [ H.txt [%param str] ]]
  | Pdf w -> [%workflow_expr picture `svg [%path svg_of_pdf w] ]
  | Svg w -> [%workflow_expr picture `svg [%path w] ]
  | Png w -> [%workflow_expr picture `png [%path w] ]
  | Section s -> [%workflow_expr H.h2 [ H.txt [%param s] ]]
  | Subsection s -> [%workflow_expr H.h3 [ H.txt [%param s] ]]

let%pworkflow render nb =
  let cells = [%eval Workflow.list @@ List.map nb.cells ~f:render_cell] in
  let head_contents = [
    H.link ~rel:[`Stylesheet] ~href:"https://unpkg.com/marx-css/css/marx.min.css" () ;
    H.meta ~a:[H.a_name "viewport" ; H.a_content "width=device-width, initial-scale=1"] () ;
  ]
  in
  let body_contents = [
    H.main (
      H.h1 [ H.txt nb.title ] :: H.hr () :: cells
    ) ;
  ]
  in
  let doc = H.html (H.head (H.title (H.txt nb.title)) head_contents) (H.body body_contents) in
  Out_channel.with_file [%dest] ~f:(fun oc ->
      Tyxml_html.pp () (Format.formatter_of_out_channel oc) doc
    )
