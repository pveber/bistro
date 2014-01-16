(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    pflag ["ocaml"; "compile"] "I" (fun x -> S [A "-I"; A x]);
    flag ["ocamldep"; "ocaml"; "use_bistro_syntax"]
      (S [A "-ppopt"; P "src/syntax/bistro_syntax.cma"]);
    flag ["compile"; "ocaml"; "use_bistro_syntax"]
      (S [A "-ppopt"; P "src/syntax/bistro_syntax.cma"])
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
