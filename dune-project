(lang dune 3.2)
(using menhir 2.0)
(cram enable)

(name melitte)
(version 0.0.1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Adrien Guatto")
(maintainers "Adrien Guatto")

(license GPL-3.0-or-later)

(package
 (name melitte)
 (synopsis "A toy implementation of Martin-Löf Type Theory")
 (description "")
 (depends (ocaml (> 4.14.0)) dune pprint menhir ppx_deriving sexplib sedlex))
