open Core
open Bistro

module Sys = Sys_unix

let fetch_pwm_archive : [`cisbp] directory =
  Bistro_unix.wget Biotk.Cisbp.pwm_archive_url
  |> Bistro_unix.unzip
  |> Fn.flip Workflow.select ["pwms"]

let fetch_tf_information : tsv file =
  Bistro_unix.wget Biotk.Cisbp.tf_information_archive_url
  |> Bistro_unix.unzip
  |> Fn.flip Workflow.select ["TF_Information_all_motifs.txt"]

type annotated_motif = {
  id : string ;
  tf_name : string ;
  pwm : Biotk.Pwm.t ;
  rc_pwm : Biotk.Pwm.t ;
  threshold : float ;
  infos : Biotk.Cisbp.TF_information.item list ;
}

let annotated_motifs =
  let f = fun%workflow () ->
    let open Biotk.Cisbp in
    let motifs =
      Motif.read_all_in_dir [%path fetch_pwm_archive]
    in
    let motif_info =
      TF_information.from_file [%path fetch_tf_information]
      |> List.filter ~f:(fun mi -> String.equal mi.TF_information.tf_species "Mus_musculus")
      |> List.filter_map ~f:(fun mi ->
          Option.map mi.TF_information.motif_id ~f:(fun id -> id, mi)
        )
      |> String.Map.of_alist_multi
    in
    let selected_motifs =
      List.filter_map motifs ~f:(fun (label, motif) ->
          let id = Filename.chop_extension label in
          Map.find motif_info id
          |> Option.bind ~f:(fun mis ->
              let tf_names =
                List.map mis ~f:(fun mi -> mi.TF_information.tf_name)
                |> List.dedup_and_sort ~compare:String.compare
              in
              match tf_names with
              | tf_name :: _ -> Some (label, tf_name, motif, mis)
              | [] -> None
            )
        )
    in
    let n = List.length selected_motifs in
    List.mapi selected_motifs ~f:(fun i (id, tf_name, motif, infos) ->
        let pwm = Motif.pwm motif in
        let threshold = Biotk.Pwm_stats.TFM_pvalue.score_of_pvalue pwm (Biotk.Pwm.flat_background ()) 1e-4 in
        let rc_pwm = Biotk.Pwm.reverse_complement pwm in
        ignore (
          Sys.command (sprintf "echo %s %s %d/%d >> delme" Time_float_unix.(now () |> to_string) tf_name i n)
          : int
        ) ;
        { id ; tf_name ; pwm ; rc_pwm ; threshold ; infos }
      )
  in
  Workflow.plugin ~descr:"cisbp.annotated_motifs" f
