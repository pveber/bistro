open Bistro

let of_bed3 ~feature_type ~attribute_type (bed : #bed3 file) : gff file =
  let f = fun%workflow dest ->
    let feature_type, attribute_type =
      [%param feature_type, attribute_type] in
    let open Biotk.Pipe_parsers in
    run (
      from_file [%path bed]
      $$ bed_parser ()
      $$ filter_map (fun (chr, start_pos, stop_pos, _) ->
          let r = Biocaml_base.Gff.{
              seqname = chr ;
              source = None ;
              feature = Some feature_type ;
              start_pos ; stop_pos ;
              score = None ;
              strand = `Not_stranded ; phase = None ;
              attributes = [attribute_type, [Printf.sprintf "%s:%d-%d" chr start_pos stop_pos]]
            } in
          Some (`Record r)
        )
      $$ gff_unparser `three
      $$ to_file dest
    )
  in
  Workflow.path_plugin ~descr:"gff.of_bed3" f
