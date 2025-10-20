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
  (name bistro)
  (tags (workflow pipeline))
  (synopsis "A library to build and run scientific workflows")
  (description "")
  (depends
    (ocaml (>= 5.4.0))))
