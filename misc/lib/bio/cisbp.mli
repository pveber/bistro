open Bistro

val fetch_pwm_archive : [`cisbp] directory

val fetch_tf_information : tsv file

type annotated_motif = {
  id : string ;
  tf_name : string ;
  pwm : Biotk.Pwm.t ;
  rc_pwm : Biotk.Pwm.t ;
  threshold : float ;
  infos : Biotk.Cisbp.TF_information.item list ;
}

val annotated_motifs : annotated_motif list workflow
