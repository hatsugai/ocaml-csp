(lang dune 1.11)
(using fmt 1.2)
(name csp)
(source (github hatsugai/ocaml-csp))
(license "MIT")
(authors "Hisabumi Hatsugai")

(package
 (name csp)
 (synopsis "CSP library for OCaml")
 (depends
  (dune (>= 2.0.0))))

(generate_opam_files true)
