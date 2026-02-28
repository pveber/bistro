(lang dune 3.16)
(generate_opam_files true)

(name bistro)
(source (github pveber/bistro))
(homepage "https://github.com/pveber/bistro/")
(bug_reports "https://github.com/pveber/bistro/issues")
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name bistro-bio)
  (synopsis "Bistro workflows for computational biology")
  (tags (bioinformatics))
  (description "
This library provides wrappers for popular tools for genomics, transcriptomics
and phylogeny, as well as custom tools to help piping data from one tool to the
other.
")
  (depends
    (biotk (>= 0.4))
    tyxml
    bistro))

(package
  (name bistro)
  (tags (workflow pipeline))
  (synopsis "A library to build and run distributed scientific workflows")
  (description "
bistro is an OCaml library to build and run computations represented
by a collection of interdependent scripts or OCaml functions, as is
often found in data analysis.

Features:
- build complex and composable workflows declaratively
- simple and lightweight wrapping of new components
- resume-on-failure: if something fails, fix it and the workflow will
  restart from where it stopped
- parallel workflow execution
- development-friendly: when a script is modified, bistro
  automatically finds out what needs to be recomputed
- automatic naming of generated files
- static typing: detect file format errors at compile time
")
  (depends
    (base64 (>= 3.0.0))
    bos
    (dune (> 1.6))
    (core (>= 0.15.0))
    (core_unix (>= 0.15.0))
    (lwt (>= 3.2.0))
    lwt_react
    (ocamlgraph (>= 1.8.7))
    (ocaml (>= 4.08.0))
    (ppxlib (>= 0.23.0))
    ppx_sexp_conv
    rresult
    (sexplib (>= 113.24.00))
    (streaming (>= 0.8.0))
    (tyxml (>= 4.3.0))))
