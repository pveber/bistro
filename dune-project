(lang dune 1.11)
(generate_opam_files true)

(name bistro)
(source (github pveber/bistro))
(homepage "https://github.com/pveber/bistro/")
(bug_reports "https://github.com/pveber/bistro/issues")
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name bistro)
  (synopsis "A library to build and run distributed scientific workflows")
  (description "
bistro is an OCaml library to build and run computations represented
by a collection of interdependent scripts, as is often found in
data analysis (especially computational biology).

Features:
- build complex and composable workflows declaratively
- simple and lightweight wrapping of new components
- resume-on-failure: if something fails, fix it and the workflow will
  restart from where it stopped
- parallel workflow execution
- development-friendly: when a script is modified, bistro
  automatically finds out what needs to be recomputed
- automatic naming of generated files
- static typing: detect file format errors at compile time!

The library provides a datatype to represent scripts (including
metadata and dependencies), an engine to run workflows and a standard
library providing components for popular tools (although mostly
related to computational biology and unix for now).
")
  (tags (bioinformatics workflow pipeline))
  (depends
    base64
    bos
    (dune (> 1.6))
    (core (>= 0.12.0))
    (lwt (>= 3.2.0))
    lwt_react
    (ocamlgraph (>= 1.8.7))
    ppx_sexp_conv
    rresult
    (sexplib (>= 113.24.00))
    (tyxml (>= 4.0))))
