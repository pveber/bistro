open Core_kernel.Std

type t = statement list
and statement =
| Factor of factor
| sam le of sample
| Model of model
| Project of string
and factor = {
  factor_name : string ;
}
and sample = {
  sample_id : string ;
  sample_data : sample_data ;
  sample_exp : experiment ;
  sample_model : string ;
  sample_condition : (string * string) list ;
}
and sample_data = [
| `short_read_data of short_read_data
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `EM_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_data = [
| `fastq of
    [ `sanger | `solexa | `phred64 ] *
    string list se_or_pe
| `sra of
    [`single_end | `paired_end] *
    [ `SRR of string list | `file of string list ]
]
and model = {
  model_id : string ;
  model_genome : genome option ;
  model_annotation : annotation option ;
}
and genome = [
| `ucsc of ucsc_genome
| `fasta of string
]
and annotation = [
  | `ensembl of ensembl_species * int
  | `gff of string
]
and 'a se_or_pe = [
  | `single_end of 'a
  | `paired_end of 'a * 'a
]
and ucsc_genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
and ensembl_species =  [
  | `homo_sapiens
  | `mus_musculus
]
with sexp

let se_or_pe_map x ~f = match x with
  | `single_end x -> `single_end (f x)
  | `paired_end (x_1, x_2) -> `paired_end (f x_1, f x_2)

let load path =
  Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp

let save cfg path =
  Sexplib.Sexp.save_hum path (sexp_of_t cfg)

type error = [
  | `undeclared of [`factor | `model | `sample] * string
  | `multiple_declaration of [`factor | `model | `sample] * string
  | `missing_project_description
  | `more_than_one_project_description of string list
  | `missing_factor_in_sample of string * string
  | `repeated_factor_in_sample of string * string
]
with sexp

module Check(X : sig
               val config : t
             end) = struct
  open X

  let extract f = List.filter_map config ~f
  let extract_unique f = List.dedup (extract f)
  let find_dups l =
    let rec aux seen dups = function
      | [] -> dups
      | h :: t ->
        if List.mem seen h then
          aux seen (h :: dups) t
        else
          aux (h :: seen) dups t
    in
    aux [] [] l


  let projects =
    extract (function
        | Project name -> Some name
        | _ -> None
      )

  let factors = extract (
    function
    | Factor f -> Some f
    | _ -> None
  )

  let unique_factors = List.dedup factors

  let models =
    extract (
      function
      | Model m -> Some m
      | _ -> None
    )

  let model_ids = List.map models ~f:(fun m -> m.model_id)

  let samples =
    extract (
      function
      | sam le s -> Some s
      | _ -> None
    )

  let sample_ids = List.map samples ~f:(fun s -> s.sample_id)
  let factor_names = List.map factors ~f:(fun f -> f.factor_name)

  let undeclared item xs x = if List.mem xs x then None else (Some (`undeclared (item, x)))

  let undeclared_factors =
    List.map samples ~f:(fun s ->
        List.map s.sample_condition ~f:fst
        |> List.filter_map ~f:(undeclared `factor factor_names)
      )
    |> List.concat

  let undeclared_models =
    List.filter_map samples ~f:(fun s -> undeclared `model model_ids s.sample_model)

  let multiply_declared_factors = find_dups factor_names
  let multiply_declared_samples = find_dups sample_ids
  let multiply_declared_models = List.(map models ~f:(fun s -> s.model_id) |! find_dups)
  let project_declaration_error = match projects with
    | [] -> [ `missing_project_description ]
    | [ _ ] -> []
    | ids -> [ `more_than_one_project_description ids ]

  let missing_factor_in_condition c =
    let factors_in_c = List.map c ~f:fst in
    List.find unique_factors ~f:(fun f -> not (List.mem factors_in_c f.factor_name))

  let missing_factor_in_samples =
    List.filter_map samples ~f:(fun s ->
        missing_factor_in_condition s.sample_condition
        |> Option.map ~f:(fun f -> `missing_factor_in_sample (f.factor_name, s.sample_id))
      )

  let repeated_factor_in_condition c =
    List.find_a_dup (List.map c ~f:fst)

  let repeated_factor_in_samples =
    List.filter_map samples ~f:(fun s ->
        repeated_factor_in_condition s.sample_condition
        |> Option.map ~f:(fun f -> `repeated_factor_in_sample (f, s.sample_id))
      )

end

let check desc =
  let module E = Check(struct let config = desc end) in
  List.(concat [
      map E.multiply_declared_factors ~f:(fun x -> `multiple_declaration (`factor, x)) ;
      map E.multiply_declared_models ~f:(fun x -> `multiple_declaration (`model, x)) ;
      map E.multiply_declared_samples ~f:(fun x -> `multiple_declaration (`sample, x)) ;
      E.project_declaration_error ;
      E.undeclared_factors ;
      E.undeclared_models ;
      E.missing_factor_in_samples ;
      E.repeated_factor_in_samples ;
    ])

let error_msg e =
  Sexp.to_string_hum (sexp_of_error e)
