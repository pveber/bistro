open Bistro
open Formats

let of_bed3 ~feature_type ~attribute_type (bed : #bed3 file) : gff file =
  let f = fun%workflow dest ->
    let feature_type, attribute_type =
      [%param feature_type, attribute_type] in
    let open Streaming in
    let open Biotk in
    let gff_items =
      Stream.of_file [%path bed]
      |> Stream.map (fun s ->
          let { Bed.Bed3.chrom ; chromStart ; chromEnd } =
            Bed.Bed3.Item.of_line (Line.of_string_unsafe s) in
          let attributes = [attribute_type, [Printf.sprintf "%s:%d-%d" chrom chromStart chromEnd]] in
          let r =
            Gff.record
              chrom chromStart chromEnd
              ~feature:feature_type ~strand:`Not_stranded ~attributes
          in
          Gff.GFF3.Item.unparse (`Record r)
        )
    in
    Stream.to_file dest gff_items
  in
  Workflow.path_plugin ~descr:"gff.of_bed3" f
