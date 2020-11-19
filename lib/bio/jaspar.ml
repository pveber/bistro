open Core_kernel
open Bistro

let core_vertebrates_non_redundant =
  Bistro_unix.wget
    ~user_agent:{|"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.22 (KHTML, like Gecko) Ubuntu Chromium/25.0.1364.160 Chrome/25.0.1364.160 Safari/537.22"|}
    "http://jaspar.genereg.net/download/CORE/JASPAR2018_CORE_vertebrates_non-redundant_pfms_jaspar.zip"
  |> Bistro_unix.unzip

let motif_list db =
  let f = fun%workflow () ->
    let db_dir = [%path db] in
    let motifs =
      Sys.readdir db_dir
      |> Array.to_list
      |> List.filter ~f:(function
          | "." | ".." -> false
          | _ -> true
        )
      |> List.map ~f:(fun fn -> Biotk.Jaspar.of_file (Filename.concat db_dir fn))
      |> Result.all
    in
    match motifs with
    | Ok xs -> xs
    | Error msg -> failwith msg
  in
  Workflow.plugin ~descr:"jaspar.motif_list" f
